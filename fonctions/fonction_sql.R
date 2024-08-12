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
              'SELECT string_agg(idu::text, '', '') 
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
              'SELECT string_agg(idu::text, '', '') 
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
          'SELECT string_agg(idu::text, '', '') FROM %I WHERE geometry && $1 AND ST_Intersects(geometry, $1)',
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
           WHERE idu IN (SELECT unnest(string_to_array($1, '', '')))
          ',
          nom_table_avant
      );
      EXECUTE query_sql INTO enveloppe_convex_avant USING p_avant;
  
      -- Préparer et exécuter la requête pour obtenir l'enveloppe convexe après
      query_sql := format(
          'SELECT ST_ConvexHull(ST_Union(geometry))
           FROM %I
           WHERE idu IN (SELECT unnest(string_to_array($1, '', '')))
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
  CREATE OR REPLACE FUNCTION regroupement_par_com(
      idu text,
      nom_table text,
      nom_table_geom text,
      indic_avant boolean DEFAULT true
  )
  RETURNS TABLE (idu_bis text, code_com text, participants text, geom_union geometry) AS $$
  DECLARE
      sql_query text;
      participants_field text;
  BEGIN
  
      -- Choix du champ des participants selon la valeur de indic_avant
      IF indic_avant THEN
          participants_field := 'participants_avant';
      ELSE
          participants_field := 'participants_apres';
      END IF;
      
      -- Construction de la requête SQL dynamique pour les participants avant
      sql_query := '
      WITH split_strings_avant AS (
        SELECT
          idu,
          unnest(string_to_array(' || participants_field || ', '', '')) AS sub_string
        FROM ' || quote_ident(nom_table) || '
        WHERE idu = $1
      ),
      groupes_avant AS (
        SELECT
          idu,
          sub_string,
          SUBSTRING(sub_string FROM 3 FOR 3) AS code_com
        FROM
          split_strings_avant
      ),
      groupes_union_avant AS (
        SELECT
          g.idu,
          g.code_com,
          string_agg(sub_string::text, '', '') AS participants,
          ST_Union(geometry) AS geom_union
        FROM
          ' || quote_ident(nom_table_geom) || ' t
        JOIN
          groupes_avant g ON t.idu = g.sub_string
        GROUP BY
          g.code_com, g.idu
      )
      SELECT
        idu,
        code_com,
        participants,
        geom_union
      FROM
        groupes_union_avant';

      RETURN QUERY EXECUTE sql_query USING idu;
  END;
  $$ LANGUAGE plpgsql;
")

dbExecute(conn, "
  CREATE OR REPLACE FUNCTION echange_parcelles(
      idu text,
      nom_table text,
      nom_table_avant text,
      nom_table_apres text
  )
  RETURNS TABLE (
      idu_bis text,
      code_com_apres text
  ) AS $$
  BEGIN
      -- Jointure FULL entre les résultats des deux exécutions de regroupement_par_com
      RETURN QUERY 
      WITH jointure AS (
          SELECT 
              COALESCE(avant.idu_bis, apres.idu_bis) AS idu_bis,
              avant.code_com AS code_com_avant,
              avant.participants AS participants_avant,
              apres.code_com AS code_com_apres,
              apres.participants AS participants_apres,
              calcul_iou_recale(avant.geom_union, apres.geom_union) AS iou
          FROM 
              regroupement_par_com(idu, nom_table, nom_table_avant, true) AS avant
          FULL JOIN 
              regroupement_par_com(idu, nom_table, nom_table_apres, false) AS apres
          ON 
              avant.idu_bis = apres.idu_bis 
              AND avant.code_com = apres.code_com
      ),
      filtre AS (
          SELECT 
              j.idu_bis, 
              COUNT(*) FILTER (WHERE j.iou IS NOT NULL) AS count_iou_valid,
              COUNT(*) FILTER (WHERE j.iou IS NULL) AS count_iou_na,
              MIN(j.iou) AS min_iou
          FROM 
              jointure j
          GROUP BY 
              j.idu_bis
          HAVING 
              COUNT(*) > 1  -- Seuls les idu_bis apparaissant plus d'une fois
      )
      SELECT 
          f.idu_bis,
          apres.code_com
      FROM 
          filtre f
      JOIN 
          regroupement_par_com(idu, nom_table, nom_table_apres, false) AS apres
          ON 
              f.idu_bis = apres.idu_bis 
      WHERE 
          -- Si toutes les IoU sont NA
          f.count_iou_valid = 0
          OR 
          -- Si au moins un IoU est inférieur à 0.95
          (f.count_iou_valid > 0 AND f.min_iou < 0.95)
          AND  SUBSTRING(f.idu_bis FROM 3 FOR 3) <> apres.code_com;

  END;
  $$ LANGUAGE plpgsql;
")

