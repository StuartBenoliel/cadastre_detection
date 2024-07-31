traitement_parcelles <- function(conn, num_departement, temps_apres, temps_avant, nb_parcelles_seuil=5e05) {
  dbExecute(conn, paste0(
    "DROP SCHEMA IF EXISTS traitement_", params$temps_apres, "_", params$temps_avant, "_cadastre_", params$num_departement, " CASCADE;"))
  source(file = "database/table_traitement_db.R")
  
  dbExecute(conn, paste0("
  INSERT INTO ajout
  SELECT idu, nom_com, code_com, com_abs, contenance, geometry
  FROM parc_", params$num_departement, "_", params$temps_apres, " apres
  WHERE NOT EXISTS (
    SELECT 1
    FROM parc_", params$num_departement, "_", params$temps_avant," avant
    WHERE apres.idu = avant.idu
  );
"))
  
  dbExecute(conn, paste0("
  INSERT INTO supp
  SELECT idu, nom_com, code_com, com_abs, contenance, geometry
  FROM parc_", params$num_departement, "_", params$temps_avant, " avant
  WHERE NOT EXISTS (
    SELECT 1
    FROM parc_", params$num_departement, "_", params$temps_apres," apres
    WHERE avant.idu = apres.idu
  );
"))
  
  dbExecute(conn, paste0("
  INSERT INTO bordure
  SELECT
      code_insee, nom_com,
      ST_Multi(
          ST_Simplify(
              ST_Difference(
                  ST_Buffer(geometry, 250), ST_Buffer(geometry, -250)
              )
          , 10)
      ) AS geometry
  FROM
      com_", params$num_departement, ";"
  ))
  
  dbExecute(conn, paste0("
  INSERT INTO identique
  SELECT apres.idu, apres.nom_com, avant.nom_com, apres.code_com
  FROM parc_", params$num_departement, "_", params$temps_apres, " apres
  FULL JOIN parc_", params$num_departement, "_", params$temps_avant, " avant 
    ON avant.idu = apres.idu
  WHERE EXISTS (
      SELECT 1
      FROM parc_", params$num_departement, "_", params$temps_avant, " avant
      WHERE apres.idu = avant.idu
      AND safe_st_equals(
          ST_SnapToGrid(apres.geometry, 0.0001), 
          ST_SnapToGrid(avant.geometry, 0.0001)
      )
  );"))
  
  dbExecute(conn, paste0(" 
  INSERT INTO cas_disparition_commune
  SELECT DISTINCT
      nom_com_apres,
      code_com,
      nom_com_avant,
      code_com
  FROM identique 
  WHERE nom_com_apres != nom_com_avant;"))
  
  nb_parcelles <- dbGetQuery(conn, paste0("
    SELECT 
      (SELECT COUNT(*) FROM parc_", params$num_departement, "_", params$temps_apres, ") - 
      (SELECT COUNT(*) FROM identique) AS parcelles_non_identique
  "))
  
  if (nb_parcelles$parcelles_non_identique > nb_parcelles_seuil) {
    print(paste0("Traitement département ", params$num_departement, 
                 " pour les années 20", params$temps_apres, " - 20", params$temps_avant, 
                 " partiel. Cause : nombre de parcelles à traiter est trop volumineux : ", 
                 nb_parcelles$parcelles_non_identique, 
                 " (contre un seuil de ", nb_parcelles_seuil, "). Fin de la partie traitement pour les parcelles ayant le même identifiant. Lancer le traitement en mode manuel ou changer le seuil sinon !"))
    
  } else {
    dbExecute(conn, paste0("
  INSERT INTO modif_avant
  SELECT avant.idu, avant.nom_com, avant.code_com, avant.com_abs, 
      avant.contenance, avant.geometry
  FROM parc_", params$num_departement, "_", params$temps_avant, " avant
  WHERE NOT EXISTS (
      SELECT 1
      FROM identique
      WHERE avant.idu = identique.idu
  )
  AND NOT EXISTS (
      SELECT 1
      FROM supp
      WHERE avant.idu = supp.idu
  );"))
    
    dbExecute(conn, paste0("
  INSERT INTO modif_apres
  SELECT apres.idu, apres.nom_com, apres.code_com, apres.com_abs, 
      apres.contenance, apres.geometry
  FROM parc_", params$num_departement, "_", params$temps_apres, " apres
  WHERE NOT EXISTS (
      SELECT 1
      FROM identique
      WHERE apres.idu = identique.idu
  )
  AND NOT EXISTS (
      SELECT 1
      FROM ajout
      WHERE apres.idu = ajout.idu
  );"))
    
    dbExecute(conn, "
  INSERT INTO modif
  SELECT
      COALESCE(avant.idu, apres.idu) AS idu,
      avant.geometry AS geometry_avant,
      apres.geometry AS geometry_apres,
      calcul_iou(apres.geometry, avant.geometry) AS iou
  FROM
      modif_avant avant
  FULL JOIN
      modif_apres apres ON avant.idu = apres.idu;
")
    
    dbExecute(conn, "
  DELETE FROM modif
  WHERE iou >= 0.99 OR iou IS NULL;
")
    
    dbExecute(conn, "
  INSERT INTO modif_iou_ajust
  SELECT
      idu, iou,
      calcul_iou_ajust(geometry_apres, geometry_avant) AS iou_ajust
  FROM
      modif;
")
    
    dbExecute(conn, "
  INSERT INTO modif_avant_iou
  SELECT avant.idu, avant.nom_com, avant.code_com, avant.com_abs, 
      avant.contenance, ajust.iou, ajust.iou_ajust, avant.geometry
  FROM modif_iou_ajust ajust
  LEFT JOIN modif_avant avant ON ajust.idu = avant.idu;
")
    
    dbExecute(conn, "
  INSERT INTO modif_apres_iou
  SELECT apres.idu, apres.nom_com, apres.code_com, apres.com_abs, 
      apres.contenance, ajust.iou, ajust.iou_ajust, apres.geometry
  FROM modif_iou_ajust ajust
  LEFT JOIN modif_apres apres ON ajust.idu = apres.idu;
")
    
    dbExecute(conn, "
  INSERT INTO translation
  SELECT idu, nom_com, code_com, com_abs, contenance, iou_ajust, 
      idu AS idu_translate, geometry
  FROM modif_apres_iou
  WHERE iou_ajust >= 0.99;
")
    
    dbExecute(conn, "
  INSERT INTO contour
  SELECT idu, nom_com, code_com, com_abs, contenance,
         iou AS iou_multi,
         idu AS participants_avant,
         idu AS participants_apres,
         geometry
  FROM modif_avant_iou
  WHERE iou >= 0.95 AND iou_ajust < 0.99;
")
    
    dbExecute(conn, "
  INSERT INTO contour_translation
  SELECT idu, nom_com, code_com, com_abs, contenance, iou_ajust, 
      idu AS idu_translate, geometry
  FROM modif_apres_iou
  WHERE iou < 0.95 AND iou_ajust >= 0.95 AND iou_ajust < 0.99;
")
    
    dbExecute(conn, "
  DELETE FROM modif_avant_iou
  WHERE EXISTS (
      SELECT 1
      FROM translation
      WHERE modif_avant_iou.idu = translation.idu
  ) OR EXISTS (
      SELECT 1
      FROM contour
      WHERE modif_avant_iou.idu = contour.idu
  ) OR EXISTS (
      SELECT 1
      FROM contour_translation
      WHERE modif_avant_iou.idu = contour_translation.idu
  );
")
    
    dbExecute(conn, "
  DELETE FROM modif_apres_iou
  WHERE EXISTS (
      SELECT 1
      FROM translation
      WHERE modif_apres_iou.idu = translation.idu
  ) OR EXISTS (
      SELECT 1
      FROM contour
      WHERE modif_apres_iou.idu = contour.idu
  ) OR EXISTS (
      SELECT 1
      FROM contour_translation
      WHERE modif_apres_iou.idu = contour_translation.idu
  );
")
    
    dbExecute(conn, "TRUNCATE TABLE multi_calcul_cache;")
    
    dbExecute(conn, "
  INSERT INTO modif_avant_iou_multi
  SELECT idu, nom_com, code_com, com_abs, contenance, iou, iou_ajust,
    (calcul_iou_multi(idu, geometry, 'modif_avant_iou', 'modif_apres_iou')).*,
    geometry
  FROM modif_avant_iou;
")
    
    dbExecute(conn, "
  INSERT INTO contour
  SELECT idu, nom_com, code_com, com_abs, contenance,
         iou_multi, participants_avant, participants_apres, geometry
  FROM modif_avant_iou_multi
  WHERE (iou_multi >= 0.95 AND LENGTH(participants_avant) = LENGTH(participants_apres))
     OR (LENGTH(participants_avant) = 14 AND LENGTH(participants_apres) = 14 AND iou > iou_ajust);
")
    
    dbExecute(conn, "
  INSERT INTO contour_translation
  SELECT avant.idu, avant.nom_com, avant.code_com, avant.com_abs, 
      avant.contenance, avant.iou_ajust, avant.idu, apres.geometry
  FROM modif_avant_iou_multi AS avant
  LEFT JOIN modif_apres apres ON avant.idu = apres.idu
  WHERE iou_multi IS NULL;
")
    
    dbExecute(conn, "
  DELETE FROM modif_avant_iou_multi
  WHERE idu IN (SELECT unnest(regexp_split_to_array(participants_avant, ',\\s*')) FROM contour)
    OR idu IN (SELECT idu FROM contour_translation);
")
    
    dbExecute(conn, "
  DELETE FROM modif_apres_iou
  WHERE idu IN (SELECT unnest(regexp_split_to_array(participants_apres, ',\\s*')) FROM contour)
    OR idu IN (SELECT idu FROM contour_translation);
")
    
    
    dbExecute(conn, "
  INSERT INTO contour
  SELECT idu, nom_com, code_com, com_abs, contenance,
         iou_multi, participants_avant, participants_apres, geometry
  FROM modif_avant_iou_multi
  WHERE iou_ajust < (iou + 0.1);
")
    
    dbExecute(conn, "
  INSERT INTO contour_translation
  SELECT avant.idu, avant.nom_com, avant.code_com, avant.com_abs, 
         avant.contenance, avant.iou_ajust,
         avant.idu AS idu_translate, apres.geometry
  FROM modif_avant_iou_multi AS avant
  LEFT JOIN modif_apres apres ON avant.idu = apres.idu
  WHERE iou_ajust >= (iou + 0.1);
")
    
    dbExecute(conn, "
  DELETE FROM modif_avant_iou_multi
  WHERE idu IN (SELECT unnest(regexp_split_to_array(participants_avant, ',\\s*')) FROM contour)
    OR idu IN (SELECT idu FROM contour_translation);
")
    
    dbExecute(conn, "
  DELETE FROM modif_apres_iou
  WHERE idu IN (SELECT unnest(regexp_split_to_array(participants_apres, ',\\s*')) FROM contour)
    OR idu IN (SELECT idu FROM contour_translation);
")
  }
  
  
  
  
  dbExecute(conn, "
  INSERT INTO defusion_com
  WITH avant AS (
      SELECT
          idu, nom_com, code_com,
          SUBSTR(idu, 1, 2) || com_abs || '000' || SUBSTR(idu, 9, 14) AS idu_apres
      FROM
          supp
      WHERE com_abs != '000'
  )
  SELECT 
      ajout.idu, ajout.nom_com, ajout.code_com, ajout.com_abs, ajout.contenance,
         avant.idu, avant.nom_com, avant.code_com, ajout.geometry
  FROM
      ajout
  JOIN avant ON ajout.idu = avant.idu_apres;
")
  
  dbExecute(conn, paste0("
  INSERT INTO cas_disparition_commune
  SELECT defusion_com.nom_com, defusion_com.code_com, 
        defusion_com.nom_com_avant, defusion_com.code_com_avant  
        FROM defusion_com
  UNION DISTINCT
  SELECT 
      fusion_ajout.nom_com, fusion_ajout.code_com, 
      fusion_ajout.nom_com_avant, fusion_ajout.code_com_avant
  FROM (
      WITH avant AS (
          SELECT
              idu, nom_com, code_com,
              SUBSTR(idu, 1, 2) AS prefix_idu,
              SUBSTR(idu, 6, 14) AS suffix_idu
          FROM
              supp
          WHERE com_abs != '000'
      )
      SELECT 
          ajout.nom_com, ajout.code_com, 
          avant.nom_com AS nom_com_avant, avant.code_com AS code_com_avant
      FROM
          avant
      JOIN 
          ajout ON ajout.idu LIKE avant.prefix_idu || '___' || avant.suffix_idu
      WHERE avant.nom_com NOT IN (SELECT DISTINCT nom_com FROM parc_", params$num_departement, "_", params$temps_apres, ")
  ) AS fusion_ajout
  ;
"))
  
  dbExecute(conn, paste0("
  INSERT INTO defusion_com
  WITH avant AS (
      SELECT
          idu, nom_com, code_com,
          SUBSTR(idu, 1, 2) AS prefix_idu,
          SUBSTR(idu, 6, 14) AS suffix_idu
      FROM
          supp
      WHERE com_abs != '000'
  )
  SELECT 
      ajout.idu, ajout.nom_com, ajout.code_com, ajout.com_abs, ajout.contenance,
         avant.idu, avant.nom_com, avant.code_com, ajout.geometry
  FROM
      avant
  JOIN ajout ON ajout.idu LIKE avant.prefix_idu || '___' || avant.suffix_idu
  WHERE avant.nom_com IN (SELECT DISTINCT nom_com FROM parc_", params$num_departement, "_", params$temps_apres,") OR
  avant.nom_com IN (SELECT nom_com_avant FROM cas_disparition_commune GROUP BY nom_com_avant HAVING COUNT(nom_com) > 1);"))
  
  dbExecute(conn, paste0(" 
  INSERT INTO chgt_commune
  WITH defusion_data AS (
      SELECT
          nom_com_avant, 
          code_com_avant, 
          STRING_AGG(DISTINCT nom_com, ', ') AS participants,
          STRING_AGG(DISTINCT code_com, ', ') AS participants_code_com
      FROM
          defusion_com
      GROUP BY 
          nom_com_avant, code_com_avant
  ),
  com_apres AS (
      SELECT DISTINCT
          nom_com, code_com
      FROM 
          parc_", params$num_departement, "_", params$temps_apres,"
  )
  SELECT
      df.nom_com_avant,
      df.code_com_avant,
      CASE 
          WHEN c.nom_com IS NOT NULL THEN 
              'Défusion partielle'
          ELSE 
              'Défusion totale'
      END AS changement,
      df.participants,
      df.participants_code_com
  FROM
      defusion_data df
  LEFT JOIN 
      com_apres c ON c.nom_com = df.nom_com_avant
;"))
  
  dbExecute(conn, "
  INSERT INTO fusion_com
  WITH apres AS (
      SELECT
          idu, nom_com, code_com, com_abs, contenance,
          SUBSTR(idu, 1, 2) || com_abs || '000' || SUBSTR(idu, 9, 14) AS idu_avant,
          geometry
      FROM
          ajout
      WHERE com_abs != '000'
  )
  SELECT 
      apres.idu, apres.nom_com, apres.code_com, apres.com_abs, apres.contenance,
         supp.idu, supp.nom_com, supp.code_com, apres.geometry
  FROM
      apres
  JOIN supp ON apres.idu_avant = supp.idu;
")
  
  dbExecute(conn, paste0("
  INSERT INTO fusion_com
  WITH avant AS (
      SELECT
          idu, nom_com, code_com,
          SUBSTR(idu, 1, 2) AS prefix_idu,
          SUBSTR(idu, 6, 14) AS suffix_idu
      FROM
          supp
      WHERE com_abs != '000'
  )
  SELECT 
      ajout.idu, ajout.nom_com, ajout.code_com, ajout.com_abs, ajout.contenance,
         avant.idu, avant.nom_com, avant.code_com, ajout.geometry
  FROM
      avant
  JOIN ajout ON ajout.idu LIKE avant.prefix_idu || '___' || avant.suffix_idu
  WHERE avant.nom_com NOT IN (SELECT DISTINCT nom_com FROM parc_", params$num_departement, "_", params$temps_apres,") AND
  avant.nom_com IN (SELECT nom_com_avant FROM cas_disparition_commune GROUP BY nom_com_avant HAVING COUNT(nom_com) = 1);"))
  
  
  dbExecute(conn, paste0(" 
  INSERT INTO chgt_commune
  WITH fusion_data AS (
      SELECT
          nom_com, 
          code_com, 
          STRING_AGG(DISTINCT nom_com_avant, ', ') AS participants,
          STRING_AGG(DISTINCT code_com_avant, ', ') AS participants_code_com
      FROM
          fusion_com
      GROUP BY 
          nom_com, code_com
  ),
  com_avant AS (
      SELECT DISTINCT
          nom_com, code_com
      FROM 
          parc_", params$num_departement, "_", params$temps_avant,"
  )
  SELECT
      f.nom_com,
      f.code_com,
      'Fusion' AS changement,
      CASE 
          WHEN c.nom_com IS NOT NULL THEN 
              STRING_AGG(DISTINCT c.nom_com || ', ' || f.participants, ', ')
          ELSE 
              f.participants 
      END AS participants,
      CASE 
          WHEN c.nom_com IS NOT NULL THEN 
              STRING_AGG(DISTINCT c.code_com || ', ' || f.participants_code_com , ', ')
          ELSE 
              f.participants_code_com 
      END AS participants_code_com
  FROM
      fusion_data f
  LEFT JOIN 
      com_avant c ON f.nom_com = c.nom_com
  GROUP BY
      f.nom_com, c.nom_com, f.code_com, f.participants, f.participants_code_com
;"))
  
  dbExecute(conn, paste0(" 
  UPDATE chgt_commune
  SET 
      participants = cas_disparition_commune.nom_com_avant || ', ' || chgt_commune.participants,
      participants_code_com = cas_disparition_commune.code_com_avant || ', ' || chgt_commune.participants_code_com
  FROM cas_disparition_commune
  WHERE chgt_commune.nom_com = cas_disparition_commune.nom_com;
"))
  
  dbExecute(conn, paste0(" 
  INSERT INTO chgt_commune
  SELECT nom_com, code_com, 'Changement de nom', nom_com_avant, code_com_avant
  FROM cas_disparition_commune
  WHERE nom_com NOT IN (SELECT nom_com FROM chgt_commune);
;"))
  
  dbExecute(conn, "
  INSERT INTO ajout_simp
  SELECT idu, ST_SnapToGrid(geometry, 0.0001) AS geometry
  FROM ajout;
")
  
  dbExecute(conn, "
  INSERT INTO supp_simp
  SELECT idu, ST_SnapToGrid(geometry, 0.0001) AS geometry
  FROM supp;
")
  
  dbExecute(conn, "
  DELETE FROM ajout
  WHERE EXISTS (
      SELECT 1
      FROM ajout_simp
        JOIN supp_simp ON ST_Equals(ajout_simp.geometry, supp_simp.geometry)
      WHERE ajout.idu = ajout_simp.idu
       AND ST_IsValid(ajout_simp.geometry) and ST_IsValid(supp_simp.geometry)
);")
  
  dbExecute(conn, "
  DELETE FROM supp
  WHERE EXISTS (
      SELECT 1
      FROM supp_simp
        JOIN ajout_simp ON ST_Equals(ajout_simp.geometry, supp_simp.geometry)
      WHERE supp.idu = supp_simp.idu
       AND ST_IsValid(ajout_simp.geometry) and ST_IsValid(supp_simp.geometry)
);")
  
  dbExecute(conn, "
  INSERT INTO supp_iou
  SELECT
      idu, nom_com, code_com, com_abs, contenance,
      (calcul_iou_intersec(geometry, 'ajout')).*,
      geometry
  FROM
      supp;
")
  
  dbExecute(conn, "
  INSERT INTO subdiv
  SELECT *
  FROM supp_iou
  WHERE iou >= 0.99 AND LENGTH(participants) != 14;
")
  
  dbExecute(conn, "
  DELETE FROM ajout
  WHERE idu IN 
      (SELECT unnest(regexp_split_to_array(participants, ',\\s*')) 
        FROM supp_iou WHERE iou >= 0.99);
")
  
  dbExecute(conn, "
  DELETE FROM supp_iou
  WHERE iou >= 0.99;
")
  
  dbExecute(conn, "
  INSERT INTO ajout_iou
  SELECT
      idu, nom_com, code_com, com_abs, contenance,
      (calcul_iou_intersec(geometry, 'supp_iou')).*,
      geometry
  FROM
      ajout;
")
  
  dbExecute(conn, "
  INSERT INTO fusion
  SELECT *
  FROM ajout_iou
  WHERE iou >= 0.99 AND LENGTH(participants) != 14;
")
  
  dbExecute(conn, "
  DELETE FROM supp_iou
  WHERE idu IN 
      (SELECT unnest(regexp_split_to_array(participants, ',\\s*')) 
        FROM ajout_iou WHERE iou >= 0.99);
")
  
  
  dbExecute(conn, "
  DELETE FROM ajout_iou
  WHERE iou >= 0.99;
")
  
  dbExecute(conn, "
  INSERT INTO ajout_iou_translate
  SELECT DISTINCT
      idu, nom_com, code_com, com_abs, contenance, iou, participants,
      (calcul_iou_intersec_best_translate(geometry, 'supp_iou')).*,
      geometry
  FROM
      ajout_iou;
")
  
  dbExecute(conn, "
  INSERT INTO translation
  SELECT idu, nom_com, code_com, com_abs, contenance, iou_ajust, 
      idu_translate, geometry
  FROM ajout_iou_translate
  WHERE iou_ajust >= 0.99;
")
  
  dbExecute(conn, "
  INSERT INTO contour_translation
  SELECT idu, nom_com, code_com, com_abs, contenance, iou_ajust, 
      idu_translate, geometry
  FROM ajout_iou_translate
  WHERE (iou_ajust >= 0.95 AND iou_ajust < 0.99);
")
  
  dbExecute(conn, "
  DELETE FROM ajout_iou_translate
  WHERE EXISTS (
      SELECT 1
      FROM translation
      WHERE ajout_iou_translate.idu = translation.idu
  ) OR EXISTS (
      SELECT 1
      FROM contour_translation
      WHERE ajout_iou_translate.idu = contour_translation.idu
  );
")
  
  dbExecute(conn, "
  DELETE FROM supp_iou
  WHERE EXISTS (
      SELECT 1
      FROM translation
      WHERE supp_iou.idu = translation.idu_translate
  ) OR EXISTS (
      SELECT 1
      FROM contour_translation
      WHERE supp_iou.idu = contour_translation.idu_translate
  );
")
  
  dbExecute(conn, "TRUNCATE TABLE multi_calcul_cache;")
  
  dbExecute(conn, "
  INSERT INTO supp_iou_multi
  SELECT
      idu, nom_com, code_com, com_abs, contenance, iou, participants, 
      (calcul_iou_multi(idu, geometry, 'supp_iou', 'ajout_iou_translate')).*,
      geometry
  FROM
      supp_iou;
")
  
  dbExecute(conn, "
  INSERT INTO redecoupage
  SELECT idu, nom_com, code_com, com_abs, contenance, iou_multi, 
      participants_avant, participants_apres, geometry
  FROM supp_iou_multi
  WHERE iou_multi >= 0.99 AND LENGTH(participants_apres) != LENGTH(participants_avant);
")
  
  dbExecute(conn, "
  INSERT INTO contour
  SELECT idu, nom_com, code_com, com_abs, contenance, iou_multi, 
      participants_avant, participants_apres, geometry
  FROM supp_iou_multi
  WHERE iou_multi >= 0.95 AND LENGTH(participants_apres) = LENGTH(participants_avant);
")
  
  dbExecute(conn, "
  INSERT INTO contour_transfo
  SELECT idu, nom_com, code_com, com_abs, contenance, iou_multi, 
      participants_avant, participants_apres, geometry
  FROM supp_iou_multi
  WHERE (iou_multi >= 0.95 AND LENGTH(participants_apres) != LENGTH(participants_avant)
   AND iou_multi < 0.99);
")
  
  dbExecute(conn, "
  DELETE FROM ajout_iou_translate
  WHERE idu IN (SELECT unnest(regexp_split_to_array(participants_apres, ',\\s*')) FROM redecoupage)
    OR idu IN (SELECT unnest(regexp_split_to_array(participants_apres, ',\\s*')) FROM contour)
    OR idu IN (SELECT unnest(regexp_split_to_array(participants_apres, ',\\s*')) FROM contour_transfo);
")
  
  dbExecute(conn, "
  DELETE FROM supp_iou_multi
  WHERE idu IN (SELECT unnest(regexp_split_to_array(participants_avant, ',\\s*')) FROM redecoupage)
    OR idu IN (SELECT unnest(regexp_split_to_array(participants_avant, ',\\s*')) FROM contour)
    OR idu IN (SELECT unnest(regexp_split_to_array(participants_avant, ',\\s*')) FROM contour_transfo);
")
  
  dbExecute(conn, "
  INSERT INTO supp_iou_multi_translate_rapide
  SELECT 
      idu, nom_com, code_com, com_abs, contenance, iou, participants,
      iou_multi, participants_avant, participants_apres,
      (calcul_iou_multi_translate_rapide(geometry, 'supp_iou_multi', 'ajout_iou_translate')).*,
      geometry
  FROM
      supp_iou_multi;
")
  
  dbExecute(conn, "
  INSERT INTO max_iou
  SELECT idu, MAX(iou_multi_translate) AS max_iou
  FROM (
      SELECT unnest(regexp_split_to_array(participants_avant_translate, ',\\s*')) AS idu, iou_multi_translate
      FROM supp_iou_multi_translate_rapide
      WHERE iou_multi_translate >= 0.95
  ) subquery
  GROUP BY idu;
")
  
  dbExecute(conn, "
  INSERT INTO multi_translate_rapide
  SELECT DISTINCT ON (sub.idu) sub.idu, sub.iou_multi_translate, 
      sub.participants_avant_translate, sub.participants_apres_translate
  FROM (
      SELECT unnest(regexp_split_to_array(participants_avant_translate, ',\\s*')) AS idu, 
          iou_multi_translate, participants_avant_translate, participants_apres_translate
      FROM supp_iou_multi_translate_rapide
      WHERE iou_multi_translate >= 0.95
  ) sub
  JOIN max_iou mi
  ON sub.idu = mi.idu AND sub.iou_multi_translate = mi.max_iou
  ORDER BY sub.idu, sub.iou_multi_translate DESC;
")
  
  dbExecute(conn, "
  INSERT INTO contour_transfo_translation
  SELECT simtr.idu, simtr.nom_com, simtr.code_com, simtr.com_abs, 
      simtr.contenance, mtr.iou_multi_translate, 
      mtr.participants_avant_translate, mtr.participants_apres_translate, 
      simtr.geometry
  FROM multi_translate_rapide mtr
  LEFT JOIN supp_iou_multi_translate_rapide simtr ON mtr.idu = simtr.idu;
")
  
  dbExecute(conn, "
  DELETE FROM ajout_iou_translate
  WHERE idu IN 
      (SELECT unnest(regexp_split_to_array(participants_apres_translate, ',\\s*')) 
        FROM multi_translate_rapide);
")
  
  dbExecute(conn, "
  DELETE FROM supp_iou_multi_translate_rapide
  WHERE idu IN 
      (SELECT unnest(regexp_split_to_array(participants_avant_translate, ',\\s*')) 
        FROM multi_translate_rapide);
")
  
  dbExecute(conn, "TRUNCATE TABLE multi_calcul_cache;")
  
  dbExecute(conn, "
  INSERT INTO supp_iou_multi_translate
  SELECT 
      idu, nom_com, code_com, com_abs, contenance, iou, participants,
      iou_multi, participants_avant, participants_apres,
      (calcul_iou_multi_translate(idu, geometry, 'supp_iou_multi_translate_rapide', 'ajout_iou_translate')).*,
      geometry
  FROM
      supp_iou_multi_translate_rapide;
")
  
  #Probleme maintenant
  dbExecute(conn, "
  INSERT INTO contour_transfo_translation
  SELECT idu, nom_com, code_com, com_abs, contenance, iou_multi_translate, 
      participants_avant_translate, participants_apres_translate, geometry
  FROM supp_iou_multi_translate
  WHERE iou_multi_translate >= 0.95;
")
  
  dbExecute(conn, "
  DELETE FROM ajout_iou_translate
  WHERE idu IN (SELECT unnest(regexp_split_to_array(participants_apres_translate, ',\\s*')) FROM contour_transfo_translation);
")
  
  dbExecute(conn, "
  DELETE FROM supp_iou_multi_translate
  WHERE idu IN (SELECT unnest(regexp_split_to_array(participants_avant_translate, ',\\s*')) FROM contour_transfo_translation);
")
  
  dbExecute(conn, "TRUNCATE TABLE multi_calcul_cache;")
  
  dbExecute(conn, "
  INSERT INTO multi_ajout
  WITH calc_results AS (
      SELECT
          idu,
          (calcul_iou_multi(idu, geometry, 'ajout_iou_translate', 'supp_iou_multi_translate')).*
      FROM
          ajout_iou_translate
  )
  SELECT DISTINCT
      unnest(string_to_array(participants_apres, ', ')) AS idu, 
      iou_multi,
      participants_apres,
      participants_avant
  FROM
      calc_results
  WHERE iou_multi >= 0.95;
")
  
  dbExecute(conn, "
  INSERT INTO redecoupage
  SELECT simt.idu, simt.nom_com, simt.code_com, simt.com_abs, simt.contenance, 
      ma.iou_multi, ma.participants_avant, ma.participants_apres, simt.geometry
  FROM supp_iou_multi_translate simt
  LEFT JOIN multi_ajout ma ON ma.idu = simt.idu
  WHERE ma.iou_multi >= 0.99 AND LENGTH(ma.participants_apres) != LENGTH(ma.participants_avant);
")
  
  dbExecute(conn, "
  INSERT INTO contour
  SELECT simt.idu, simt.nom_com, simt.code_com, simt.com_abs, simt.contenance, 
      ma.iou_multi, ma.participants_avant, ma.participants_apres, simt.geometry
  FROM supp_iou_multi_translate simt
  LEFT JOIN multi_ajout ma ON ma.idu = simt.idu
  WHERE ma.iou_multi >= 0.95 AND LENGTH(ma.participants_apres) = LENGTH(ma.participants_avant);
")
  
  dbExecute(conn, "
  INSERT INTO contour_transfo
  SELECT simt.idu, simt.nom_com, simt.code_com, simt.com_abs, simt.contenance, 
      ma.iou_multi, ma.participants_avant, ma.participants_apres, simt.geometry
  FROM supp_iou_multi_translate simt
  LEFT JOIN multi_ajout ma ON ma.idu = simt.idu
  WHERE (ma.iou_multi >= 0.95 AND LENGTH(ma.participants_apres) != LENGTH(ma.participants_avant)
   AND ma.iou_multi < 0.99);
")
  
  dbExecute(conn, "
  DELETE FROM ajout_iou_translate
  WHERE idu IN (SELECT unnest(regexp_split_to_array(participants_apres, ',\\s*')) FROM redecoupage)
    OR idu IN (SELECT unnest(regexp_split_to_array(participants_apres, ',\\s*')) FROM contour)
    OR idu IN (SELECT unnest(regexp_split_to_array(participants_apres, ',\\s*')) FROM contour_transfo);
")
  
  dbExecute(conn, "
  DELETE FROM supp_iou_multi_translate
  WHERE idu IN (SELECT unnest(regexp_split_to_array(participants_avant, ',\\s*')) FROM redecoupage)
    OR idu IN (SELECT unnest(regexp_split_to_array(participants_avant, ',\\s*')) FROM contour)
    OR idu IN (SELECT unnest(regexp_split_to_array(participants_avant, ',\\s*')) FROM contour_transfo);
")
  dbExecute(conn, "
  INSERT INTO supp_iou_restant
  SELECT
      idu, nom_com, code_com, com_abs, contenance,
      (calcul_iou_intersec(geometry, 'ajout_iou_translate')).*,
      iou_multi, participants_avant, participants_apres, 
      iou_multi_translate, participants_avant_translate, participants_apres_translate,
      geometry
  FROM
      supp_iou_multi_translate;
")
  
  dbExecute(conn, "
  INSERT INTO ajout_iou_restant
  SELECT
      idu, nom_com, code_com, com_abs, contenance,
      (calcul_iou_intersec(geometry, 'supp_iou_multi_translate')).*, 
      iou_ajust, idu_translate, geometry
  FROM
      ajout_iou_translate;
")
  
  dbExecute(conn, "
  INSERT INTO vrai_ajout
  SELECT idu, nom_com, code_com, com_abs, contenance, iou, participants, geometry
  FROM ajout_iou_restant
  WHERE iou IS NULL;
")
  
  dbExecute(conn, "
  INSERT INTO vrai_supp
  SELECT idu, nom_com, code_com, com_abs, contenance, iou, participants, geometry
  FROM supp_iou_restant
  WHERE iou IS NULL;
")
  
  dbExecute(conn, "
  DELETE FROM ajout_iou_restant
  WHERE EXISTS (
      SELECT 1
      FROM vrai_ajout
      WHERE ajout_iou_restant.idu = vrai_ajout.idu
  );
")
  
  dbExecute(conn, "
  DELETE FROM supp_iou_restant
  WHERE EXISTS (
      SELECT 1
      FROM vrai_ajout
      WHERE supp_iou_restant.idu = vrai_ajout.idu
  );
")
  
  dbExecute(conn, "
  DROP TABLE IF EXISTS multi_calcul_cache, identique, ajout, supp, ajout_tot, supp_tot,
  modif_avant, modif_apres, modif, modif_avant_iou, ajout_simp, supp_simp, 
  supp_iou, ajout_iou, ajout_iou_translate, supp_iou_multi, 
  supp_iou_multi_translate_rapide, max_iou, cas_disparition_commune,
  multi_translate_rapide, supp_iou_multi_translate, multi_ajout CASCADE
;")
  
}