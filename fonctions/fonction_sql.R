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
  CREATE OR REPLACE FUNCTION calcul_iou_recale(polygon_1 geometry, polygon_2 geometry)
  RETURNS numeric AS $$
  DECLARE
      centre_1 geometry;
      centre_2 geometry;
      translation_x double precision;
      translation_y double precision;
      polygon_1_recale geometry;
      iou_recale numeric;
  BEGIN
      -- Vérifier si les géométries ne sont pas vides
      IF NOT ST_IsEmpty(polygon_1) AND NOT ST_IsEmpty(polygon_2) THEN
          centre_1 := ST_Centroid(polygon_1);
          centre_2 := ST_Centroid(polygon_2);
          translation_x := ST_X(centre_2) - ST_X(centre_1);
          translation_y := ST_Y(centre_2) - ST_Y(centre_1);
          
          -- Calculer la géométrie 1 translaté
          polygon_1_recale := ST_Translate(polygon_1, translation_x, translation_y);
          
          -- Calculer l'IoU recalé avec la géométrie 1 translaté
          iou_recale := calcul_iou(polygon_1_recale, polygon_2);
          
          RETURN iou_recale;
      ELSE
          RETURN NULL;
      END IF;
  END;
  $$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;
")

# Pour les parcelles avec changement d'idu
dbExecute(conn, "
  CREATE OR REPLACE FUNCTION calcul_iou_intersect( 
    polygon geometry, 
    nom_table text, 
    recale boolean DEFAULT false, 
    seuil_intersect numeric DEFAULT 0.9
  )
  RETURNS TABLE (iou numeric, participants text) AS $$
  DECLARE
      query_sql text;
      parcelles_intersectant RECORD;
      first boolean := true;
      polygon_union geometry;
      iou_fonction text;  -- Variable pour stocker le nom de la fonction de calcul de l'IoU
  BEGIN
  
      participants := '';
      
      -- Première requête pour récupérer les idu et les géométries correspondantes
      query_sql := format(
          'SELECT idu, geometry
           FROM %I
           WHERE geometry && $1 
             AND ST_Intersects($1, geometry)
             AND ST_Area(ST_Intersection($1, geometry)) / ST_Area(geometry) >= $2', 
           nom_table
      );
      
      FOR parcelles_intersectant IN EXECUTE query_sql USING polygon, seuil_intersect
      LOOP

          IF first THEN
              polygon_union := parcelles_intersectant.geometry;
              participants := parcelles_intersectant.idu::text;
              first := false;
          ELSE
              polygon_union := ST_Union(polygon_union, parcelles_intersectant.geometry);
              participants := participants || ', ' || parcelles_intersectant.idu::text;
          END IF;
      END LOOP;
      
      -- Si aucun résultat, retourner NULL
      IF participants = '' THEN
          RETURN QUERY SELECT NULL::numeric, NULL::text;
          RETURN;  -- Terminer la fonction immédiatement
      END IF;
      
      -- Choisir la fonction de calcul de l'IoU en fonction du paramètre recal
      IF recale THEN
          iou_fonction := 'calcul_iou_recale';
      ELSE
          iou_fonction := 'calcul_iou';
      END IF;
  
      -- Calculer l'IoU avec l'union des parcelles et le polygon
      EXECUTE format(
          'SELECT %s($1, $2)',
          iou_fonction
      ) INTO iou USING polygon, polygon_union;
      
      RETURN QUERY SELECT iou, participants;
  END;
  $$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;
")


dbExecute(conn, "
  CREATE OR REPLACE FUNCTION calcul_iou_multi_safe(
      idu text, 
      polygon_avant geometry, 
      nom_table text
  )
  RETURNS TABLE (iou_multi numeric, participants_avant text, participants_apres text) AS $$
  DECLARE
      polygon_union geometry := polygon_avant;
      query_sql text;
      nb_polygon_union integer;
      nb_polygon integer := 1;
      polygon_union_apres geometry;
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
          
          query_sql := format(
              'SELECT ST_Union(geometry_avant), COUNT(*)
               FROM %I
               WHERE geometry_avant && $1 AND ST_Intersects(geometry_avant, $1)',
              nom_table
          );
    
          LOOP
              -- Mettre à jour polygon_union et nb_polygon_union dans la même requête
              EXECUTE query_sql INTO polygon_union, nb_polygon_union USING polygon_union;
              
              EXIT WHEN nb_polygon = nb_polygon_union;
  
              -- Mettre à jour le nombre de polygon
              nb_polygon := nb_polygon_union;
          END LOOP;
  
          -- Sélectionner les noms des participants avant
          query_sql := format(
              'SELECT string_agg(idu::text, '','') 
               FROM %I 
               WHERE geometry_avant && $1 AND ST_Intersects(geometry_avant, $1)',
              nom_table
          );
          
          EXECUTE query_sql INTO participants_avant USING polygon_union;
          
          query_sql := format(
              'SELECT ST_Union(geometry_apres)
               FROM %I
               WHERE idu IN (
                   SELECT idu
                   FROM %I
                   WHERE geometry_avant && $1 AND ST_Intersects(geometry_avant, $1)
               )',
              nom_table,
              nom_table
          );
          
          EXECUTE query_sql INTO polygon_union_apres USING polygon_union;

          INSERT INTO multi_calcul_cache (participants_avant, participants_apres, iou_multi, participants_avant_hash)
          VALUES (participants_avant, participants_avant, calcul_iou(polygon_union, polygon_union_apres), md5(participants_avant));
          
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
      recale boolean DEFAULT false,
      seuil_intersect numeric DEFAULT 0.9
  )
  RETURNS TABLE (iou_multi numeric, participants_avant text, participants_apres text) AS $$
  DECLARE
      polygon_union geometry := polygon_avant;
      query_sql text;
      nb_polygon_union integer;
      nb_polygon integer := 1;
      iou_intersect RECORD;
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
          
          query_sql := format(
              'SELECT ST_Union(geometry), COUNT(*)
               FROM %I
               WHERE geometry && $1 AND ST_Intersects(geometry, $1)',
              nom_table_avant
          );
          -- Boucle pour trouver les intersections dans nom_table_avant
          LOOP
              -- Mettre à jour polygon_union et nb_polygon_union dans la même requête
              EXECUTE query_sql INTO polygon_union, nb_polygon_union USING polygon_union;
              
              EXIT WHEN nb_polygon = nb_polygon_union;
  
              -- Mettre à jour le nombre de polygon
              nb_polygon := nb_polygon_union;
          END LOOP;
  
          -- Sélectionner les noms des participants avant
          query_sql := format(
              'SELECT string_agg(idu::text, '','') 
               FROM %I 
               WHERE geometry && $1 AND ST_Intersects(geometry, $1)',
              nom_table_avant
          );
          
          EXECUTE query_sql INTO participants_avant USING polygon_union;
  
          -- Calculer l'IoU intersection avec nom_table_apres
          SELECT * INTO iou_intersect
          FROM calcul_iou_intersect(polygon_union, nom_table_apres, recale, seuil_intersect);
          
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

# Version non bouclée
dbExecute(conn, "
  CREATE OR REPLACE FUNCTION calcul_iou_multi_rapide(
      polygon_avant geometry, 
      nom_table_avant text, 
      nom_table_apres text,
      recale boolean DEFAULT false,
      seuil_intersect numeric DEFAULT 0.9,
      nb_iter int DEFAULT 1
  )
  RETURNS TABLE (iou_multi numeric, participants_avant text, participants_apres text) AS $$
  DECLARE
      i int;
      query_sql text;
      polygon_union geometry := polygon_avant;
      iou_intersect RECORD;
  BEGIN
  
      -- Loop for the specified number of iterations
      FOR i IN 1..nb_iter LOOP

          EXECUTE format(
              'SELECT ST_Union(geometry) FROM %I WHERE geometry && $1 AND ST_Intersects(geometry, $1)',
              nom_table_avant
          ) INTO polygon_union USING polygon_union;
          
      END LOOP;
  
      -- Get the participants before
      query_sql := format(
          'SELECT string_agg(idu::text, '','') FROM %I WHERE geometry && $1 AND ST_Intersects(geometry, $1)',
          nom_table_avant
      );
      EXECUTE query_sql INTO participants_avant USING polygon_union;
  
      -- Calculate the IoU with nom_table_apres
      SELECT * INTO iou_intersect
      FROM calcul_iou_intersect(polygon_union, nom_table_apres, recale, seuil_intersect);
  
      -- Return the results
      RETURN QUERY SELECT iou_intersect.iou, participants_avant, iou_intersect.participants;
  END;
  $$ LANGUAGE plpgsql;
") 

dbExecute(conn, "
  CREATE OR REPLACE FUNCTION calcul_iou_convex(nom_table_avant text, nom_table_apres text, p_avant text, p_apres text)
  RETURNS numeric AS $$
  DECLARE
      query_sql text;
      enveloppe_convex_avant geometry;
      enveloppe_convex_apres geometry;
      iou_convex numeric;
  BEGIN
      -- Préparer et exécuter la requête pour obtenir l'enveloppe convexe avant
      query_sql := format(
          'SELECT ST_ConvexHull(ST_Union(geometry))
           FROM %I
           WHERE idu IN (SELECT unnest(string_to_array($1, '','')))
          ',
          nom_table_avant
      );
      EXECUTE query_sql INTO enveloppe_convex_avant USING p_avant;
  
      -- Préparer et exécuter la requête pour obtenir l'enveloppe convexe après
      query_sql := format(
          'SELECT ST_ConvexHull(ST_Union(geometry))
           FROM %I
           WHERE idu IN (SELECT unnest(string_to_array($1, '','')))
          ',
          nom_table_apres
      );
      EXECUTE query_sql INTO enveloppe_convex_apres USING p_apres;

      -- Calculer l'IoU
      iou_convex := calcul_iou(enveloppe_convex_avant, enveloppe_convex_apres);
      RETURN iou_convex;
  END;
  $$ LANGUAGE plpgsql;
")


dbExecute(conn, "
  CREATE OR REPLACE FUNCTION calcul_iou_intersect_best_recale(polygon geometry, nom_table text, seuil_qualite numeric default 0.01)
  RETURNS TABLE (iou_recale numeric, idu_recale text) AS $$
  DECLARE
      query_sql text;
      parcelles_intersectant RECORD;
      max_iou numeric := NULL;
      iou numeric;
  BEGIN
      -- Sélectionner les parcelles intersectant le polygon
      
      query_sql := format(
          'SELECT idu, geometry
           FROM %I
           WHERE geometry && $1 
             AND ST_Intersects($1, geometry)
             AND ABS(contenance - ST_Area($1)) / ST_Area($1) <= $2', 
           nom_table
      );
      
      FOR parcelles_intersectant IN EXECUTE query_sql USING polygon, seuil_qualite
      LOOP
          -- Calculer l'IoU entre la parcelle courante et polygon
          iou := calcul_iou(parcelles_intersectant.geometry, polygon);
          
          -- Mettre à jour si on trouve un IoU plus élevé
          IF max_iou IS NULL OR iou > max_iou THEN
              max_iou := iou;
              idu_recale := parcelles_intersectant.idu;
          END IF;
      END LOOP;
      
      -- Si aucune parcelle intersecte, retourner NA
      IF max_iou IS NULL THEN
          RETURN QUERY SELECT NULL::numeric, NULL::text;
      END IF;
      
      query_sql := format(
          'SELECT geometry
           FROM %I
           WHERE idu = $1', 
           nom_table
      );
      
      -- Calculer l'IoU ajusté avec la meilleure parcelle trouvée
      EXECUTE query_sql INTO parcelles_intersectant.geometry USING idu_recale;
      iou_recale := calcul_iou_recale(parcelles_intersectant.geometry, polygon);
      
      -- Retourner les résultats
      RETURN QUERY SELECT iou_recale, idu_recale;
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

dbExecute(conn, "
  CREATE OR REPLACE FUNCTION echange_parcelles_bis(
      idu text,
      nom_table text,
      nom_table_avant text,
      nom_table_apres text
  )
  RETURNS TABLE (idu_avant text, code_com_avant text, code_com_apres text) AS $$
  DECLARE
      sql_query_avant text;
      sql_query_apres text;
      code_com_commun_avant text;
      code_com_commun_apres text;
      geom_union_avant geometry;
      geom_union_apres geometry;
      iou float;
  BEGIN
      -- Construction de la requête SQL dynamique pour les participants avant
      sql_query_avant := '
      WITH RECURSIVE split_strings_avant AS (
        SELECT
          idu,
          unnest(string_to_array(participants_avant, '','')) AS sub_string
        FROM ' || quote_ident(nom_table) || '
      ),
      groupes_avant AS (
        SELECT
          idu,
          SUBSTRING(sub_string FROM 3 FOR 3) AS code_com
        FROM
          split_strings_avant
      ),
      groupes_union_avant AS (
        SELECT
          g.code_com,
          ST_Union(geometry) AS geom_union
        FROM
          ' || quote_ident(nom_table_avant) || ' t
        JOIN
          groupes_avant g ON t.idu = g.idu
        GROUP BY
          g.code_com
      )
      SELECT
        code_com,
        geom_union
      FROM
        groupes_union_avant';
        
      -- Exécution de la requête dynamique pour les participants avant
      FOR code_com_commun_avant, geom_union_avant IN EXECUTE sql_query_avant LOOP
          RAISE NOTICE 'code_com_commun avant: %', code_com_commun_avant;
          
          -- Construction de la requête SQL dynamique pour les participants après
          sql_query_apres := '
          WITH RECURSIVE split_strings_apres AS (
            SELECT
              idu,
              unnest(string_to_array(participants_apres, '','')) AS sub_string
            FROM ' || quote_ident(nom_table) || '
          ),
          groupes_apres AS (
            SELECT
              idu,
              SUBSTRING(sub_string FROM 3 FOR 3) AS code_com
            FROM
              split_strings_apres
          ),
          groupes_union_apres AS (
            SELECT
              g.code_com,
              ST_Union(geometry) AS geom_union
            FROM
              ' || quote_ident(nom_table_apres) || ' t
            JOIN
              groupes_apres g ON t.idu = g.idu
            GROUP BY
              g.code_com
          )
          SELECT
            code_com,
            geom_union
          FROM
            groupes_union_apres';
      
          -- Exécution de la requête dynamique pour les participants après
          FOR code_com_commun_apres, geom_union_apres IN EXECUTE sql_query_apres LOOP
              RAISE NOTICE 'code_com_commun apres: %', code_com_commun_apres;
              
              -- Calcul de l'indicateur (iou)
              iou := calcul_iou_recale(geom_union_avant, geom_union_apres);
              RAISE NOTICE 'IoU entre % et %: %', code_com_commun_avant, code_com_commun_apres, iou;
              
              -- Vérification des seuils et renvoi des résultats
              IF iou < 0.95 THEN
                  RETURN QUERY
                  SELECT
                    g_avant.idu,
                    g_avant.code_com AS code_com_avant,
                    g_apres.code_com AS code_com_apres
                  FROM
                    (SELECT * FROM groupes_avant WHERE code_com = code_com_commun_avant) g_avant
                  JOIN
                    (SELECT * FROM groupes_apres WHERE code_com = code_com_commun_apres) g_apres ON true;
              END IF;
          END LOOP;
      END LOOP;
      
      RETURN;
  END;
  $$ LANGUAGE plpgsql;
")



