dbExecute(conn, "
  CREATE OR REPLACE FUNCTION calcul_iou(polygon_1 geometry, polygon_2 geometry)
  RETURNS numeric AS $$
  DECLARE
      intersection geometry;
      aire_intersection numeric;
      aire_union numeric;
      iou numeric;
  BEGIN
      -- Vérifier si les géométries ne sont pas vides
      IF NOT ST_IsEmpty(polygon_1) AND NOT ST_IsEmpty(polygon_2) THEN
          intersection := ST_Intersection(polygon_1, polygon_2);
        
          -- Si l'intersection est vide, IoU = 0
          IF ST_IsEmpty(intersection) THEN
              RETURN 0;
          END IF;
        
          aire_intersection := ST_Area(intersection);
          aire_union := ST_Area(ST_Union(polygon_1, polygon_2));
        
          -- Calculer l'IoU
          iou := aire_intersection / aire_union;
          RETURN ROUND(iou, 3);
      ELSE
          RETURN NULL;
      END IF;
  END;
  $$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;
")

dbExecute(conn, "
  CREATE OR REPLACE FUNCTION calcul_iou_ajust(polygon_1 geometry, polygon_2 geometry)
  RETURNS numeric AS $$
  DECLARE
      centroid_1 geometry;
      centroid_2 geometry;
      translation_vector_x double precision;
      translation_vector_y double precision;
      polygon_1_ajust geometry;
      iou_ajust numeric;
  BEGIN
      -- Vérifier si les géométries ne sont pas vides
      IF NOT ST_IsEmpty(polygon_1) AND NOT ST_IsEmpty(polygon_2) THEN
          centroid_1 := ST_Centroid(polygon_1);
          centroid_2 := ST_Centroid(polygon_2);
          translation_vector_x := ST_X(centroid_2) - ST_X(centroid_1);
          translation_vector_y := ST_Y(centroid_2) - ST_Y(centroid_1);
          
          -- Calculer la géométrie 1 ajustée en utilisant ST_Translate
          polygon_1_ajust := ST_Translate(polygon_1, translation_vector_x, translation_vector_y);
          
          -- Calculer l'IoU ajusté avec la géométrie 1 ajustée
          iou_ajust := calcul_iou(polygon_1_ajust, polygon_2);
          
          RETURN iou_ajust;
      ELSE
          RETURN NULL;
      END IF;
  END;
  $$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;
")

dbExecute(conn, "
  CREATE OR REPLACE FUNCTION calcul_iou_intersec( 
    polygon geometry, 
    table_name text, 
    seuil_qualite numeric DEFAULT 0.9
  )
  RETURNS TABLE (iou numeric, participants text) AS $$
  DECLARE
      parcelles_intersectant RECORD;
      nom_participants text := '';
      polygon_union geometry;
      query_sql text;
      intersect_area numeric;
      polygon_area numeric;
      quality numeric;
      first boolean := true;
  BEGIN
      -- Sélectionner les parcelles intersectant le polygon
      query_sql := '
          SELECT idu, ST_Area(ST_Intersection($1, geometry)) AS intersect_area, ST_Area(geometry) AS polygon_area
          FROM ' || quote_ident(table_name) || '
          WHERE geometry && $1 AND ST_Intersects($1, geometry)
      ';
      
      FOR parcelles_intersectant IN EXECUTE query_sql USING polygon
      LOOP
          intersect_area := parcelles_intersectant.intersect_area;
          polygon_area := parcelles_intersectant.polygon_area;
          quality := intersect_area / polygon_area;
          
          IF quality >= seuil_qualite THEN
              IF first THEN
                  nom_participants := parcelles_intersectant.idu::text;
                  first := false;  -- Après le premier cas, mettre à jour la variable
              ELSE
                  nom_participants := nom_participants || ', ' || parcelles_intersectant.idu::text;
              END IF;
          END IF;
      END LOOP;
      
      -- Si aucun résultat, retourner NULL
      IF nom_participants = '' THEN
          RETURN QUERY SELECT NULL::numeric, NULL::text;
          RETURN;  -- Terminer la fonction immédiatement
      END IF;
      
      -- Calculer l'union des parcelles intersectant
      query_sql := '
          SELECT ST_Union(geometry)
          FROM ' || quote_ident(table_name) || '
          WHERE idu IN (
              SELECT idu
              FROM ' || quote_ident(table_name) || '
              WHERE geometry && $1 AND ST_Intersects($1, geometry)
              AND ST_Area(ST_Intersection($1, geometry)) / ST_Area(geometry) >= $2
          )
      ';
      EXECUTE query_sql INTO polygon_union USING polygon, seuil_qualite;
      
      -- Calculer l'IoU avec l'union des parcelles et le polygon
      RETURN QUERY SELECT calcul_iou(polygon_union, polygon), nom_participants;
  END;
  $$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;
")

dbExecute(conn, "
  CREATE OR REPLACE FUNCTION calcul_iou_multi_safe(
      idu text, 
      polygon_avant geometry, 
      nom_table_avant text, 
      nom_table_apres text
  )
  RETURNS TABLE (iou_multi numeric, participants_avant text, participants_apres text) AS $$
  DECLARE
      nb_polygon integer := 0;
      nb_polygon_union integer := 0;
      polygon_union geometry := polygon_avant;
      polygon_union_apres geometry;
      query_sql text;
  BEGIN
      -- Vérifier si les résultats sont déjà pour cet idu dans le cache
      BEGIN
        SELECT mcc.iou_multi, mcc.participants_avant, mcc.participants_apres
        INTO iou_multi, participants_avant, participants_apres
        FROM multi_calcul_cache mcc
        WHERE EXISTS (
            SELECT 1
            FROM unnest(regexp_split_to_array(mcc.participants_avant, ',\\s*')) AS participant
            WHERE participant = idu
        );
      END;
      
      -- Si les résultats ne sont pas trouvés dans le cache, exécuter le calcul
      IF NOT FOUND THEN
          -- Boucle pour trouver les intersections dans nom_table_avant
          LOOP
              -- Mettre à jour polygon_union avec l'union des géométries de nom_table_avant
              EXECUTE '
                  SELECT ST_Union(geometry) 
                  FROM ' || quote_ident(nom_table_avant) || ' 
                  WHERE geometry&&$1 AND ST_Intersects(geometry, $1)
              ' INTO polygon_union USING polygon_union;
  
              -- Sortir de la boucle si le nombre de parcelles reste le même
              query_sql := '
                  SELECT COUNT(*) 
                  FROM ' || quote_ident(nom_table_avant) || ' 
                  WHERE geometry&&$1 AND ST_Intersects(geometry, $1)';
              
              EXECUTE query_sql INTO nb_polygon_union USING polygon_union;
              
              EXIT WHEN nb_polygon = nb_polygon_union;
  
              -- Mettre à jour le nombre de polygon
              nb_polygon := nb_polygon_union;
          END LOOP;
  
          -- Sélectionner les noms des participants avant
          query_sql := '
              SELECT string_agg(idu::text, '', '') 
              FROM ' || quote_ident(nom_table_avant) || ' 
              WHERE geometry&&$1 AND ST_Intersects(geometry, $1)';
          
          EXECUTE query_sql INTO participants_avant USING polygon_union;
          
          query_sql := '
              SELECT ST_Union(geometry)  
              FROM ' || quote_ident(nom_table_apres) || ' 
              WHERE idu IN (
                  SELECT idu 
                  FROM  ' || quote_ident(nom_table_avant) || ' 
                  WHERE geometry&&$1 AND ST_Intersects(geometry, $1)
              )';
          
          EXECUTE query_sql INTO polygon_union_apres USING polygon_union;
  
          -- Insérer les résultats dans le cache seulement si participants_avant n'est pas NULL
          IF participants_avant IS NOT NULL THEN
              INSERT INTO multi_calcul_cache (participants_avant, participants_apres, iou_multi, participants_avant_hash)
              VALUES (participants_avant, participants_avant, calcul_iou(polygon_union, polygon_union_apres), md5(participants_avant::text));
          END IF;
          
          -- Récupérer les résultats pour la sortie
          RETURN QUERY SELECT calcul_iou(polygon_union, polygon_union_apres), participants_avant, participants_avant;
      ELSE 
          -- Retourner les résultats trouvés dans le cache
          RETURN QUERY SELECT iou_multi, participants_avant, participants_apres;
      END IF;
  END;
  $$ LANGUAGE plpgsql;
") 

# Non symétrique
dbExecute(conn, "
  CREATE OR REPLACE FUNCTION calcul_iou_multi(
      idu text, 
      polygon_avant geometry, 
      nom_table_avant text, 
      nom_table_apres text,
      seuil_qualite numeric DEFAULT 0.9
  )
  RETURNS TABLE (iou_multi numeric, participants_avant text, participants_apres text) AS $$
  DECLARE
      nb_polygon integer := 0;
      nb_polygon_union integer := 0;
      polygon_union geometry := polygon_avant;
      iou_intersect RECORD;
      query_sql text;
  BEGIN
      -- Vérifier si les résultats sont déjà pour cet idu dans le cache
      BEGIN
        SELECT mcc.iou_multi, mcc.participants_avant, mcc.participants_apres
        INTO iou_multi, participants_avant, participants_apres
        FROM multi_calcul_cache mcc
        WHERE EXISTS (
            SELECT 1
            FROM unnest(regexp_split_to_array(mcc.participants_avant, ',\\s*')) AS participant
            WHERE participant = idu
        );
      END;
      
      -- Si les résultats ne sont pas trouvés dans le cache, exécuter le calcul
      IF NOT FOUND THEN
          -- Boucle pour trouver les intersections dans nom_table_avant
          LOOP
              -- Mettre à jour polygon_union avec l'union des géométries de nom_table_avant
              EXECUTE '
                  SELECT ST_Union(geometry) 
                  FROM ' || quote_ident(nom_table_avant) || ' 
                  WHERE geometry&&$1 AND ST_Intersects(geometry, $1)
              ' INTO polygon_union USING polygon_union;
  
              -- Sortir de la boucle si le nombre de parcelles reste le même
              query_sql := '
                  SELECT COUNT(*) 
                  FROM ' || quote_ident(nom_table_avant) || ' 
                  WHERE geometry&&$1 AND ST_Intersects(geometry, $1)';
              
              EXECUTE query_sql INTO nb_polygon_union USING polygon_union;
              
              EXIT WHEN nb_polygon = nb_polygon_union;
  
              -- Mettre à jour le nombre de polygon
              nb_polygon := nb_polygon_union;
          END LOOP;
  
          -- Sélectionner les noms des participants avant
          query_sql := '
              SELECT string_agg(idu::text, '', '') 
              FROM ' || quote_ident(nom_table_avant) || ' 
              WHERE geometry&&$1 AND ST_Intersects(geometry, $1)';
          
          EXECUTE query_sql INTO participants_avant USING polygon_union;
  
          -- Calculer l'IoU intersection avec nom_table_apres
          SELECT * INTO iou_intersect
          FROM calcul_iou_intersec(polygon_union, nom_table_apres, seuil_qualite);
  
          -- Insérer les résultats dans le cache seulement si participants_avant n'est pas NULL
          IF participants_avant IS NOT NULL THEN
              INSERT INTO multi_calcul_cache (participants_avant, participants_apres, iou_multi, participants_avant_hash)
              VALUES (participants_avant, iou_intersect.participants, iou_intersect.iou, md5(participants_avant::text));
          END IF;
          
          -- Récupérer les résultats pour la sortie
          RETURN QUERY SELECT iou_intersect.iou, participants_avant, iou_intersect.participants;
      ELSE 
          -- Retourner les résultats trouvés dans le cache
          RETURN QUERY SELECT iou_multi, participants_avant, participants_apres;
      END IF;
  END;
  $$ LANGUAGE plpgsql;
") 

dbExecute(conn, "
  CREATE OR REPLACE FUNCTION calcul_iou_multi_test(
      idu text, 
      polygon_avant geometry, 
      nom_table_avant text, 
      nom_table_apres text,
      seuil_qualite numeric DEFAULT 0.9
  )
  RETURNS TABLE (iou_multi numeric, participants_avant text, participants_apres text) AS $$
  DECLARE
      nb_polygon integer := 0;
      nb_polygon_union integer := 0;
      polygon_union geometry := polygon_avant;
      iou_intersect RECORD;
      query_sql text;
  BEGIN
      -- Vérifier si les résultats sont déjà pour cet idu dans le cache
      BEGIN
        SELECT mcc.iou_multi, mcc.participants_avant, mcc.participants_apres
        INTO iou_multi, participants_avant, participants_apres
        FROM multi_calcul_cache mcc
        WHERE EXISTS (
            SELECT 1
            FROM unnest(regexp_split_to_array(mcc.participants_avant, ',\\s*')) AS participant
            WHERE participant = idu
        );
      END;
      
      -- Si les résultats ne sont pas trouvés dans le cache, exécuter le calcul
      IF NOT FOUND THEN
          -- Boucle pour trouver les intersections dans nom_table_avant
          LOOP
              -- Mettre à jour polygon_union avec l'union des géométries de nom_table_avant
              EXECUTE '
                  SELECT ST_Union(geometry) 
                  FROM ' || quote_ident(nom_table_avant) || ' 
                  WHERE geometry&&$1 AND ST_Intersects(geometry, $1)
              ' INTO polygon_union USING polygon_union;
  
              -- Sortir de la boucle si le nombre de parcelles reste le même
              query_sql := '
                  SELECT COUNT(*) 
                  FROM ' || quote_ident(nom_table_avant) || ' 
                  WHERE geometry&&$1 AND ST_Intersects(geometry, $1)';
              
              EXECUTE query_sql INTO nb_polygon_union USING polygon_union;
              
              EXIT WHEN nb_polygon = nb_polygon_union;
  
              -- Mettre à jour le nombre de polygon
              nb_polygon := nb_polygon_union;
          END LOOP;
  
          -- Sélectionner les noms des participants avant
          query_sql := '
              SELECT string_agg(idu::text, '', '') 
              FROM ' || quote_ident(nom_table_avant) || ' 
              WHERE geometry&&$1 AND ST_Intersects(geometry, $1)';
          
          EXECUTE query_sql INTO participants_avant USING polygon_union;
  
          -- Calculer l'IoU intersection avec nom_table_apres
          SELECT * INTO iou_intersect
          FROM calcul_iou_intersec(polygon_union, nom_table_apres, seuil_qualite);
  
          -- Insérer les résultats dans le cache seulement si participants_avant n'est pas NULL
          IF participants_avant IS NOT NULL THEN
              INSERT INTO multi_calcul_cache (participants_avant, participants_apres, iou_multi, participants_avant_hash)
              VALUES (participants_avant, iou_intersect.participants, iou_intersect.iou, md5(participants_avant::text));
          END IF;
          
          -- Récupérer les résultats pour la sortie
          RETURN QUERY SELECT iou_intersect.iou, participants_avant, iou_intersect.participants;
      ELSE 
          -- Retourner les résultats trouvés dans le cache
          RETURN QUERY SELECT iou_multi, participants_avant, participants_apres;
      END IF;
  END;
  $$ LANGUAGE plpgsql;
") 

# Version sans boucle for
dbExecute(conn, "
  CREATE OR REPLACE FUNCTION calcul_iou_multi_rapide(polygon_avant geometry, nom_table_avant text, nom_table_apres text)
  RETURNS TABLE (iou_multi numeric, participants_avant text, participants_apres text) AS $$
  DECLARE
      polygon_union geometry := polygon_avant;
      iou_intersect RECORD;
      query_sql text;
  BEGIN
      -- Mettre à jour polygon_union avec l'union des géométries de nom_table_avant
      EXECUTE '
          SELECT ST_Union(geometry) 
          FROM ' || quote_ident(nom_table_avant) || ' 
          WHERE geometry&&$1 AND ST_Intersects(geometry, $1)
      ' INTO polygon_union USING polygon_union;

      -- Sélectionner les noms des participants avant
      query_sql := '
          SELECT string_agg(idu::text, '', '') 
          FROM ' || quote_ident(nom_table_avant) || ' 
          WHERE geometry&&$1 AND ST_Intersects(geometry, $1)';
      
      EXECUTE query_sql INTO participants_avant USING polygon_avant;

      -- Calculer l'IoU intersection avec nom_table_apres
      SELECT * INTO iou_intersect
      FROM calcul_iou_intersec(polygon_union, nom_table_apres);
      
      -- Récupérer les résultats pour la sortie
      RETURN QUERY SELECT iou_intersect.iou, participants_avant, iou_intersect.participants;
  END;
  $$ LANGUAGE plpgsql;
") 


dbExecute(conn, "
  CREATE OR REPLACE FUNCTION calcul_iou_convex(nom_table_avant text, nom_table_apres text, p_avant text, p_apres text)
  RETURNS numeric AS $$
  DECLARE
      enveloppe_convex_avant geometry;
      enveloppe_convex_apres geometry;
      iou_convex numeric;
      query_sql text;
  BEGIN
      -- Obtenir les géométries convexes avant
      query_sql := '
          SELECT ST_ConvexHull(ST_Union(geometry))
          FROM ' || quote_ident(nom_table_avant) || ' 
          WHERE idu IN (SELECT unnest(string_to_array($1, '', '')))
      ';
      EXECUTE query_sql INTO enveloppe_convex_avant USING p_avant;

      -- Obtenir les géométries après
      query_sql := '
          SELECT ST_ConvexHull(ST_Union(geometry))
          FROM ' || quote_ident(nom_table_apres) || ' 
          WHERE idu IN (SELECT unnest(string_to_array($1, '', '')))
      ';
      EXECUTE query_sql INTO enveloppe_convex_apres USING p_apres;

      -- Calculer l'IoU
      IF NOT ST_IsEmpty(enveloppe_convex_avant) AND NOT ST_IsEmpty(enveloppe_convex_apres) THEN
          iou_convex := calcul_iou(enveloppe_convex_avant, enveloppe_convex_apres);
          RETURN iou_convex;
      ELSE
          RETURN NULL;
      END IF;
  END;
  $$ LANGUAGE plpgsql;
")

dbExecute(conn, "
  CREATE OR REPLACE FUNCTION calcul_iou_intersec_translate(
      polygon geometry, 
      table_name text, 
      seuil_qualite numeric DEFAULT 0
  )
  RETURNS TABLE (iou_ajust numeric, participants text) AS $$
  DECLARE
      parcelles_intersectant RECORD;
      nom_participants text := '';
      polygon_union geometry;
      query_sql text;
      intersect_area numeric;
      polygon_area numeric;
      quality numeric;
      first boolean := true;
  BEGIN
      -- Sélectionner les parcelles intersectant le polygone
      query_sql := '
          SELECT idu, ST_Area(ST_Intersection($1, geometry)) AS intersect_area, ST_Area(geometry) AS polygon_area
          FROM ' || quote_ident(table_name) || '
          WHERE geometry && $1 AND ST_Intersects($1, geometry)
      ';
      
      FOR parcelles_intersectant IN EXECUTE query_sql USING polygon
      LOOP
          intersect_area := parcelles_intersectant.intersect_area;
          polygon_area := parcelles_intersectant.polygon_area;
          quality := intersect_area / polygon_area;
          
          IF quality >= seuil_qualite THEN
              IF first THEN
                  nom_participants := parcelles_intersectant.idu::text;
                  first := false;  -- Après le premier cas, mettre à jour la variable
              ELSE
                  nom_participants := nom_participants || ', ' || parcelles_intersectant.idu::text;
              END IF;
          END IF;
      END LOOP;
      
      -- Si aucun résultat, retourner NULL
      IF nom_participants = '' THEN
          RETURN QUERY SELECT NULL::numeric, NULL::text;
          RETURN;  -- Terminer la fonction immédiatement
      END IF;
      
      -- Calculer l'union des parcelles intersectantes
      query_sql := '
          SELECT ST_Union(geometry)
          FROM ' || quote_ident(table_name) || '
          WHERE idu IN (
              SELECT idu
              FROM ' || quote_ident(table_name) || '
              WHERE geometry && $1 AND ST_Intersects($1, geometry)
              AND ST_Area(ST_Intersection($1, geometry)) / ST_Area(geometry) >= $2
          )
      ';
      EXECUTE query_sql INTO polygon_union USING polygon, seuil_qualite;
      
      -- Calculer l'IoU avec l'union des parcelles et le polygon
      RETURN QUERY SELECT calcul_iou_ajust(polygon_union, polygon), nom_participants;
  END;
  $$ LANGUAGE plpgsql;
")

dbExecute(conn, "
  CREATE OR REPLACE FUNCTION calcul_iou_intersec_best_translate(polygon geometry, table_name text, seuil_qualite numeric default 0.01)
  RETURNS TABLE (iou_ajust numeric, idu_translate text) AS $$
  DECLARE
      parcelles_intersectant RECORD;
      max_iou numeric := NULL;
      iou numeric;
      query_sql text;
  BEGIN
      -- Sélectionner les parcelles intersectant le polygon
      query_sql := '
          SELECT idu, geometry
          FROM ' || quote_ident(table_name) || '
          WHERE geometry&&$1 AND ST_Intersects(geometry, $1)
          AND ABS(contenance - ST_Area($1)) / ST_Area($1) <= $2
      ';
      
      FOR parcelles_intersectant IN EXECUTE query_sql USING polygon, seuil_qualite
      LOOP
          -- Calculer l'IoU entre la parcelle courante et polygon
          BEGIN
              iou := calcul_iou(parcelles_intersectant.geometry, polygon);
          EXCEPTION WHEN others THEN
              iou := NULL; -- Ignorer les erreurs de calcul d'IoU
          END;
          
          -- Mettre à jour si on trouve un IoU plus élevé
          IF max_iou IS NULL OR iou > max_iou THEN
              max_iou := iou;
              idu_translate := parcelles_intersectant.idu;
          END IF;
      END LOOP;
      
      -- Si aucune parcelle intersecte, retourner NA
      IF max_iou IS NULL THEN
          RETURN QUERY SELECT NULL::numeric, NULL::text;
      END IF;
      
      query_sql := '
          SELECT geometry
          FROM ' || quote_ident(table_name) || '
          WHERE idu = $1
      ';
      
      -- Calculer l'IoU ajusté avec la meilleure parcelle trouvée
      EXECUTE query_sql INTO parcelles_intersectant.geometry USING idu_translate;
      iou_ajust := calcul_iou_ajust(parcelles_intersectant.geometry, polygon);
      
      -- Retourner les résultats
      RETURN QUERY SELECT iou_ajust, idu_translate;
  END;
  $$ LANGUAGE plpgsql;
")

dbExecute(conn, "
  CREATE OR REPLACE FUNCTION calcul_iou_multi_translate(idu text, polygon_avant geometry, nom_table_avant text, nom_table_apres text)
  RETURNS TABLE (iou_multi_translate numeric, participants_avant_translate text, participants_apres_translate text) AS $$
  DECLARE
      nb_polygon integer := 0;
      nb_polygon_union integer := 0;
      polygon_union geometry := polygon_avant;
      iou_intersect RECORD;
      query_sql text;
  BEGIN
      -- Vérifier si les résultats sont déjà calculés pour ce idu dans le cache
      BEGIN
        SELECT mcc.iou_multi, 
              mcc.participants_avant, 
              mcc.participants_apres
        INTO iou_multi_translate, participants_avant_translate, participants_apres_translate
        FROM multi_calcul_cache mcc
        WHERE EXISTS (
            SELECT 1
            FROM unnest(regexp_split_to_array(mcc.participants_avant, ',\\s*')) AS participant
            WHERE participant = idu
        );
        -- Retourner les résultats trouvés dans le cache
      END;
      
      -- Si les résultats ne sont pas trouvés dans le cache, exécuter le calcul
      IF NOT FOUND THEN
          -- Boucle pour trouver les intersections dans nom_table_avant
          LOOP
              -- Mettre à jour polygon_union avec l'union des géométries de nom_table_avant
              EXECUTE '
                  SELECT ST_Union(geometry) 
                  FROM ' || quote_ident(nom_table_avant) || ' 
                  WHERE geometry&&$1 AND ST_Intersects(geometry, $1)
              ' INTO polygon_union USING polygon_union;
  
              -- Sortir de la boucle si le nombre de parcelles reste le même
              query_sql := '
                  SELECT COUNT(*) 
                  FROM ' || quote_ident(nom_table_avant) || ' 
                  WHERE geometry&&$1 AND ST_Intersects(geometry, $1)';
              
              EXECUTE query_sql INTO nb_polygon_union USING polygon_union;
              
              EXIT WHEN nb_polygon = nb_polygon_union;
  
              -- Mettre à jour le nombre de parcelles
              nb_polygon := nb_polygon_union;
          END LOOP;
  
          -- Sélectionner les noms des participants avant
          query_sql := '
              SELECT string_agg(idu::text, '', '') 
              FROM ' || quote_ident(nom_table_avant) || ' 
              WHERE geometry&&$1 AND ST_Intersects(geometry, $1)';
          
          EXECUTE query_sql INTO participants_avant_translate USING polygon_union;
  
          -- Calculer l'IoU intersection ajusté avec ajout
          EXECUTE 'SELECT * FROM calcul_iou_intersec_translate($1, $2)' INTO iou_intersect USING polygon_union, nom_table_apres;
            
          -- Insérer les résultats dans le cache seulement si participants_avant n'est pas NULL
          IF participants_avant_translate IS NOT NULL THEN
              INSERT INTO multi_calcul_cache (participants_avant, participants_apres, iou_multi, participants_avant_hash)
              VALUES (participants_avant_translate, iou_intersect.participants, iou_intersect.iou_ajust, md5(participants_avant_translate::text));
          END IF;
          
          -- Retourner les résultats
      RETURN QUERY SELECT iou_intersect.iou_ajust AS iou, participants_avant_translate AS participants_avant, iou_intersect.participants AS participants_apres;
      ELSE 
          RETURN QUERY SELECT iou_multi_translate, participants_avant_translate, participants_apres_translate;
      END IF;
  END;
  $$ LANGUAGE plpgsql;
") 

dbExecute(conn, "
  CREATE OR REPLACE FUNCTION safe_st_equals(geom1 geometry, geom2 geometry)
  RETURNS BOOLEAN AS $$
  BEGIN
    RETURN ST_Equals(geom1, geom2);
  EXCEPTION
    WHEN others THEN
      RETURN FALSE;
  END;
  $$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;
")

dbExecute(conn, "
  CREATE OR REPLACE FUNCTION calcul_iou_multi_translate_rapide(polygon_avant geometry, nom_table_avant text, nom_table_apres text)
  RETURNS TABLE (iou_multi_translate numeric, participants_avant_translate text, participants_apres_translate text) AS $$
  DECLARE
      polygon_union geometry := polygon_avant;
      nom_participants_avant text := '';
      iou_intersect RECORD;
      query_sql text;
  BEGIN
      -- Étape 1: Union de tous les polygones
      EXECUTE '
          SELECT ST_Union(geometry) 
          FROM ' || quote_ident(nom_table_avant) || ' 
          WHERE geometry&&$1 AND ST_Intersects(geometry, $1)
      ' INTO polygon_union USING polygon_union;
  
      -- Sélectionner les noms des participants avant
      query_sql := '
          SELECT string_agg(idu::text, '', '') 
          FROM ' || quote_ident(nom_table_avant) || ' 
          WHERE geometry&&$1 AND ST_Intersects(geometry, $1)';
      
      EXECUTE query_sql INTO participants_avant_translate USING polygon_avant;
  
      -- Calculer l'IoU intersection ajusté avec ajout
      EXECUTE 'SELECT * FROM calcul_iou_intersec_translate($1, $2)' INTO iou_intersect USING polygon_union, nom_table_apres;
  
      -- Retourner les résultats
      RETURN QUERY SELECT iou_intersect.iou_ajust AS iou, participants_avant_translate AS participants_avant, iou_intersect.participants AS participants_apres;
  END;
  $$ LANGUAGE plpgsql;
")

dbExecute(conn, "
  CREATE OR REPLACE FUNCTION echange_parcelles(idu_participants text, nom_table text)
  RETURNS TABLE (idu_avant text, nom_com_avant text, code_com_avant text, code_com_apres text) AS $$
  DECLARE
      sql_query text;
  BEGIN
      -- Construction de la requête SQL dynamique
      sql_query := '
      WITH RECURSIVE split_strings AS (
        SELECT
          idu,
          nom_com,
          code_com,
          unnest(string_to_array(' || quote_ident(idu_participants) || ', '', '')) AS sub_string
        FROM ' || quote_ident(nom_table) || '
      ),
      filtered AS (
        SELECT
          idu,
          nom_com,
          code_com,
          SUBSTRING(sub_string FROM 3 FOR 3) AS code_com_apres
        FROM
          split_strings
        WHERE
          SUBSTRING(sub_string FROM 3 FOR 3) <> code_com
      )
      SELECT idu AS idu_avant, nom_com AS nom_com_avant, 
          code_com AS code_com_avant, code_com_apres
      FROM
        filtered';
      
      -- Exécution de la requête dynamique
      RETURN QUERY EXECUTE sql_query;
  END;
  $$ LANGUAGE plpgsql;
")
