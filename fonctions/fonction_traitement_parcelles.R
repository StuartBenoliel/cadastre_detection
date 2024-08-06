traitement_parcelles <- function(conn, num_departement, temps_apres, temps_avant, nb_parcelles_seuil=5e05) {
  dbExecute(conn, paste0(
    "DROP SCHEMA IF EXISTS traitement_", params$temps_apres, "_", params$temps_avant, "_cadastre_", params$num_departement, " CASCADE;"))
  source(file = "database/table_traitement_db.R")
  
  dbExecute(conn, paste0("
  INSERT INTO ajout (idu, nom_com, code_com, com_abs, contenance, geometry)
  SELECT idu, nom_com, code_com, com_abs, contenance, geometry
  FROM parc_", params$num_departement, "_", params$temps_apres, " apres
  WHERE NOT EXISTS (
    SELECT 1
    FROM parc_", params$num_departement, "_", params$temps_avant," avant
    WHERE apres.idu = avant.idu
  );
"))
  
  dbExecute(conn, paste0("
  INSERT INTO supp (idu, nom_com, code_com, com_abs, contenance, geometry)
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
  INSERT INTO disparition_com
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
  INSERT INTO modif_avant (idu, nom_com, code_com, com_abs, contenance, geometry)
  SELECT avant.idu, avant.nom_com, avant.code_com, avant.com_abs, 
      avant.contenance, avant.geometry AS geometry
  FROM parc_", params$num_departement, "_", params$temps_avant, " avant
  WHERE NOT EXISTS (
      SELECT 1
      FROM identique
      WHERE avant.idu = identique.idu
  ) AND NOT EXISTS (
      SELECT 1
      FROM supp
      WHERE avant.idu = supp.idu
  );"))
    
    dbExecute(conn, paste0("
  INSERT INTO modif_apres (idu, nom_com, code_com, com_abs, contenance, geometry)
  SELECT apres.idu, apres.nom_com, apres.code_com, apres.com_abs, 
      apres.contenance, apres.geometry
  FROM parc_", params$num_departement, "_", params$temps_apres, " apres
  WHERE NOT EXISTS (
      SELECT 1
      FROM identique
      WHERE apres.idu = identique.idu
  ) AND NOT EXISTS (
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
  UPDATE modif
  SET iou_ajust = calcul_iou_ajust(m.geometry_apres, m.geometry_avant)
  FROM modif AS m
  WHERE modif.idu = m.idu;
")
    
    
    dbExecute(conn, "
  UPDATE modif_avant
  SET iou = modif.iou,
      iou_ajust = modif.iou_ajust
  FROM modif
  WHERE modif_avant.idu = modif.idu;
")
    
    
    dbExecute(conn, "
  UPDATE modif_apres
  SET iou = modif.iou,
      iou_ajust = modif.iou_ajust
  FROM modif
  WHERE modif_apres.idu = modif.idu;
")
    
    
    dbExecute(conn, "
  DELETE FROM modif_avant
  WHERE idu NOT IN (SELECT idu FROM modif);
")
    
    dbExecute(conn, "
  DELETE FROM modif_apres
  WHERE idu NOT IN (SELECT idu FROM modif);
")
    
    dbExecute(conn, "
  INSERT INTO translation
  SELECT idu, nom_com, code_com, com_abs, contenance, iou_ajust, 
      idu AS idu_translate, geometry
  FROM modif_apres
  WHERE iou_ajust >= 0.99;
")
    
    dbExecute(conn, "
  INSERT INTO contour
  SELECT idu, nom_com, code_com, com_abs, contenance,
         iou AS iou_multi,
         idu AS participants_avant,
         idu AS participants_apres,
         geometry
  FROM modif_avant
  WHERE (iou >= 0.95 OR iou_ajust >= 0.95) AND iou_ajust < 0.99;
")
    
    dbExecute(conn, "
  DELETE FROM modif_avant
  WHERE EXISTS (
      SELECT 1
      FROM translation
      WHERE modif_avant.idu = translation.idu
  ) OR EXISTS (
      SELECT 1
      FROM contour
      WHERE modif_avant.idu = contour.idu
  );
")
    
    dbExecute(conn, "
  DELETE FROM modif_apres
  WHERE EXISTS (
      SELECT 1
      FROM translation
      WHERE modif_apres.idu = translation.idu
  ) OR EXISTS (
      SELECT 1
      FROM contour
      WHERE modif_apres.idu = contour.idu
  );
")
    
    dbExecute(conn, "
  WITH updated_values AS (
    SELECT idu,
           (calcul_iou_multi_safe(idu, geometry, 'modif_avant', 'modif_apres')).*
    FROM modif_avant
  )
  UPDATE modif_avant
  SET iou_multi = updated_values.iou_multi,
      participants_avant = updated_values.participants_avant,
      participants_apres = updated_values.participants_apres
  FROM updated_values
  WHERE modif_avant.idu = updated_values.idu")
    
    dbExecute(conn, "
  INSERT INTO contour
  SELECT idu, nom_com, code_com, com_abs, contenance,
         iou_multi, participants_avant, participants_apres, geometry
  FROM modif_avant;
")
    
    
    dbExecute(conn, "
  DELETE FROM modif_avant
  WHERE idu IN (SELECT unnest(regexp_split_to_array(participants_avant, ',\\s*')) FROM contour);
")
    
    dbExecute(conn, "
  DELETE FROM modif_apres
  WHERE idu IN (SELECT unnest(regexp_split_to_array(participants_apres, ',\\s*')) FROM contour);
")
  }
  
  dbExecute(conn, "
  INSERT INTO scission_com
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
  INSERT INTO scission_com
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
  WHERE avant.nom_com IN (
      SELECT DISTINCT nom_com 
      FROM parc_", params$num_departement, "_", params$temps_apres,");"))
  
  dbExecute(conn, paste0(" 
  INSERT INTO chgt_com
  WITH scission_data AS (
      SELECT
          nom_com_avant, 
          code_com_avant, 
          STRING_AGG(DISTINCT nom_com, ', ') AS participants,
          STRING_AGG(DISTINCT code_com, ', ') AS participants_code_com
      FROM
          scission_com
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
      scission_data df
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
  INSERT INTO disparition_com
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
      WHERE avant.nom_com NOT IN (
          SELECT DISTINCT nom_com 
          FROM parc_", params$num_departement, "_", params$temps_apres, 
                         ")
  ) AS fusion_ajout;"))
  
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
  WHERE avant.nom_com NOT IN (
      SELECT DISTINCT nom_com 
      FROM parc_", params$num_departement, "_", params$temps_apres,
                         ") AND avant.nom_com IN (
      SELECT nom_com_avant 
      FROM disparition_com GROUP BY nom_com_avant HAVING COUNT(nom_com) = 1);"))
  
  
  dbExecute(conn, paste0(" 
  INSERT INTO chgt_com
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
  UPDATE chgt_com
  SET 
      participants = disparition_com.nom_com_avant || ', ' || chgt_com.participants,
      participants_code_com = disparition_com.code_com_avant || ', ' || chgt_com.participants_code_com
  FROM disparition_com
  WHERE chgt_com.nom_com = disparition_com.nom_com;
"))
  
  dbExecute(conn, paste0(" 
  INSERT INTO chgt_com
  SELECT nom_com, code_com, 'Changement de nom', nom_com_avant, code_com_avant
  FROM disparition_com
  WHERE nom_com NOT IN (SELECT nom_com FROM chgt_com);
;"))
  
  dbExecute(conn, "
  INSERT INTO ajout_simp
  SELECT idu, nom_com, code_com, ST_SnapToGrid(geometry, 0.0001) AS geometry
  FROM ajout;")
  
  dbExecute(conn, "
  INSERT INTO supp_simp
  SELECT idu, nom_com, code_com, ST_SnapToGrid(geometry, 0.0001) AS geometry
  FROM supp;")
  
  dbExecute(conn, "
  INSERT INTO identique_bis
  SELECT supp.idu, supp.nom_com, supp.code_com, 
      ajout.idu, ajout.nom_com, ajout.code_com
  FROM ajout_simp ajout
   JOIN supp_simp  supp
    ON ST_Equals(ajout.geometry, supp.geometry)
      WHERE ST_IsValid(ajout.geometry) 
          AND ST_IsValid(supp.geometry);")
  
  # Parcelles ajoutées n'ayant pas été modifiées
  dbExecute(conn, "
  DELETE FROM ajout
  WHERE EXISTS (
      SELECT 1
      FROM identique_bis ident
      WHERE ajout.idu = ident.idu_apres
  );")
  
  # Parcelles supprimées n'ayant pas été modifiées
  dbExecute(conn, "
  DELETE FROM supp
  WHERE EXISTS (
      SELECT 1
      FROM identique_bis ident
      WHERE supp.idu = ident.idu_avant
  );")
  
  
  dbExecute(conn, "
  WITH updated_values AS (
    SELECT idu,
           (calcul_iou_intersec(geometry, 'ajout')).*
    FROM supp
  )
  UPDATE supp
  SET iou = updated_values.iou,
      participants = updated_values.participants
  FROM updated_values
  WHERE supp.idu = updated_values.idu")
  
  dbExecute(conn, "
  INSERT INTO redecoupage
  SELECT idu, nom_com, code_com, com_abs, contenance, iou, 
      idu, participants, geometry
  FROM supp
  WHERE iou >= 0.99 AND LENGTH(participants) != 14;")
  
  dbExecute(conn, "
  DELETE FROM ajout
  WHERE idu IN (
      SELECT unnest(regexp_split_to_array(participants, ',\\s*')) 
      FROM supp WHERE iou >= 0.99
  );")
  
  dbExecute(conn, "
  DELETE FROM supp
  WHERE iou >= 0.99;")
  
  dbExecute(conn, "
  WITH updated_values AS (
    SELECT idu,
           (calcul_iou_intersec(geometry, 'supp')).*
    FROM ajout
  )
  UPDATE ajout
  SET iou = updated_values.iou,
      participants = updated_values.participants
  FROM updated_values
  WHERE ajout.idu = updated_values.idu")
  
  dbExecute(conn, "
  INSERT INTO fusion_ajout
  SELECT DISTINCT
      unnest(string_to_array(participants, ', ')), 
      iou,
      participants,
      idu
  FROM
      ajout
  WHERE iou >= 0.99;")
  
  dbExecute(conn, "
  INSERT INTO redecoupage
  SELECT supp.idu, supp.nom_com, supp.code_com, supp.com_abs, supp.contenance, fa.iou, 
      fa.participants_avant, fa.participants_apres, supp.geometry
  FROM supp
  LEFT JOIN fusion_ajout fa ON fa.idu = supp.idu
  WHERE fa.iou >= 0.99 AND LENGTH(fa.participants_avant) != 14;")
  
  dbExecute(conn, "
  DELETE FROM supp
  WHERE idu IN (
      SELECT unnest(regexp_split_to_array(participants, ',\\s*')) 
      FROM ajout WHERE iou >= 0.99
  );")
  
  dbExecute(conn, "
  DELETE FROM ajout
  WHERE iou >= 0.99;")
  
  dbExecute(conn, "
  WITH updated_values AS (
    SELECT idu,
           (calcul_iou_intersec_best_translate(geometry, 'supp')).*
    FROM ajout
  )
  UPDATE ajout
  SET iou_ajust = updated_values.iou_ajust,
      idu_translate = updated_values.idu_translate
  FROM updated_values
  WHERE ajout.idu = updated_values.idu")
  
  dbExecute(conn, "
  INSERT INTO translation
  SELECT idu, nom_com, code_com, com_abs, contenance, iou_ajust, 
      idu_translate, geometry
  FROM ajout
  WHERE iou_ajust >= 0.99;")
  
  dbExecute(conn, "
  DELETE FROM ajout
  WHERE EXISTS (
      SELECT 1
      FROM translation
      WHERE ajout.idu = translation.idu
  );")
  
  dbExecute(conn, "
  DELETE FROM supp
  WHERE EXISTS (
      SELECT 1
      FROM translation
      WHERE supp.idu = translation.idu_translate
  );")
  
  dbExecute(conn, "
  WITH updated_values AS (
    SELECT idu,
           (calcul_iou_multi_rapide(geometry, 'supp', 'ajout')).*
    FROM supp
  )
  UPDATE supp
  SET iou_multi = updated_values.iou_multi,
      participants_avant = updated_values.participants_avant,
      participants_apres = updated_values.participants_apres
  FROM updated_values
  WHERE supp.idu = updated_values.idu")
  
  
  dbExecute(conn, "
  INSERT INTO max_iou
  SELECT idu, MAX(iou_multi) AS max_iou
  FROM (
      SELECT unnest(regexp_split_to_array(participants_avant, ',\\s*')) AS idu, 
          iou_multi
      FROM supp
      WHERE iou_multi >= 0.99
  ) subquery
  GROUP BY idu;")
  
  dbExecute(conn, "
  INSERT INTO multi_rapide
  SELECT DISTINCT ON (sub.idu) sub.idu, sub.iou_multi, 
      sub.participants_avant, sub.participants_apres
  FROM (
      SELECT unnest(regexp_split_to_array(participants_avant, ',\\s*')) AS idu, 
          iou_multi, participants_avant, participants_apres
      FROM supp
      WHERE iou_multi >= 0.99
  ) sub
  JOIN max_iou mi
  ON sub.idu = mi.idu AND sub.iou_multi = mi.max_iou
  ORDER BY sub.idu, sub.iou_multi DESC;")
  
  
  dbExecute(conn, "
  INSERT INTO redecoupage
  SELECT supp.idu, supp.nom_com, supp.code_com, supp.com_abs, supp.contenance, 
      mtr.iou_multi, mtr.participants_avant, mtr.participants_apres, supp.geometry
  FROM multi_rapide mtr
  LEFT JOIN supp ON mtr.idu = supp.idu 
  WHERE LENGTH(mtr.participants_apres) != LENGTH(mtr.participants_avant);")
  
  dbExecute(conn, "
  INSERT INTO contour
  SELECT supp.idu, supp.nom_com, supp.code_com, supp.com_abs, supp.contenance, 
      mtr.iou_multi, mtr.participants_avant, mtr.participants_apres, supp.geometry
  FROM multi_rapide mtr
  LEFT JOIN supp ON mtr.idu = supp.idu 
  WHERE LENGTH(mtr.participants_apres) = LENGTH(mtr.participants_avant);")
  
  dbExecute(conn, "
  DELETE FROM ajout
  WHERE idu IN (
      SELECT unnest(regexp_split_to_array(participants_apres, ',\\s*')) 
      FROM multi_rapide
  );")
  
  dbExecute(conn, "
  DELETE FROM supp
  WHERE idu IN (
      SELECT unnest(regexp_split_to_array(participants_avant, ',\\s*')) 
      FROM multi_rapide
  );")
  
  dbExecute(conn, "TRUNCATE TABLE multi_calcul_cache;")
  
  dbExecute(conn, "
  WITH updated_values AS (
    SELECT idu,
           (calcul_iou_multi(idu, geometry, 'supp', 'ajout')).*
    FROM supp
  )
  UPDATE supp
  SET iou_multi = updated_values.iou_multi,
      participants_avant = updated_values.participants_avant,
      participants_apres = updated_values.participants_apres
  FROM updated_values
  WHERE supp.idu = updated_values.idu")
  
  dbExecute(conn, "
  INSERT INTO redecoupage
  SELECT idu, nom_com, code_com, com_abs, contenance, iou_multi, 
      participants_avant, participants_apres, geometry
  FROM supp
  WHERE iou_multi >= 0.99 
      AND LENGTH(participants_apres) != LENGTH(participants_avant);")
  
  dbExecute(conn, "
  INSERT INTO contour_redecoupage
  SELECT idu, nom_com, code_com, com_abs, contenance, iou_multi, 
      participants_avant, participants_apres, geometry
  FROM supp
  WHERE (iou_multi >= 0.95 
      AND LENGTH(participants_apres) != LENGTH(participants_avant) 
      AND iou_multi < 0.99
  );")
  
  dbExecute(conn, "
  INSERT INTO contour
  SELECT idu, nom_com, code_com, com_abs, contenance, iou_multi, 
      participants_avant, participants_apres, geometry
  FROM supp
  WHERE iou_multi >= 0.95 
      AND LENGTH(participants_apres) = LENGTH(participants_avant);")
  
  dbExecute(conn, "
  DELETE FROM ajout
  WHERE idu IN (
      SELECT unnest(regexp_split_to_array(participants_apres, ',\\s*')) 
      FROM redecoupage
  ) OR idu IN (
      SELECT unnest(regexp_split_to_array(participants_apres, ',\\s*')) 
      FROM contour
  ) OR idu IN (
      SELECT unnest(regexp_split_to_array(participants_apres, ',\\s*')) 
      FROM contour_redecoupage
  );")
  
  dbExecute(conn, "
  DELETE FROM supp
  WHERE idu IN (
      SELECT unnest(regexp_split_to_array(participants_avant, ',\\s*')) 
      FROM redecoupage
  ) OR idu IN (
      SELECT unnest(regexp_split_to_array(participants_avant, ',\\s*')) 
      FROM contour
  ) OR idu IN (
      SELECT unnest(regexp_split_to_array(participants_avant, ',\\s*')) 
      FROM contour_redecoupage
  );")
  
  dbExecute(conn, "TRUNCATE TABLE max_iou, multi_rapide;")
  
  dbExecute(conn, "
  WITH updated_values AS (
    SELECT idu,
           (calcul_iou_multi_translate_rapide(geometry, 'supp', 'ajout')).*
    FROM supp
  )
  UPDATE supp
  SET iou_multi_translate = updated_values.iou_multi_translate,
      participants_avant_translate = updated_values.participants_avant_translate,
      participants_apres_translate = updated_values.participants_apres_translate
  FROM updated_values
  WHERE supp.idu = updated_values.idu")
  
  dbExecute(conn, "
  INSERT INTO max_iou
  SELECT idu, MAX(iou_multi_translate) AS max_iou
  FROM (
      SELECT unnest(regexp_split_to_array(participants_avant_translate, ',\\s*')) AS idu, 
          iou_multi_translate
      FROM supp
      WHERE iou_multi_translate >= 0.95
  ) subquery
  GROUP BY idu;")
  
  dbExecute(conn, "
  INSERT INTO multi_rapide
  SELECT DISTINCT ON (sub.idu) sub.idu, sub.iou_multi_translate, 
      sub.participants_avant_translate, sub.participants_apres_translate
  FROM (
      SELECT unnest(regexp_split_to_array(participants_avant_translate, ',\\s*')) AS idu, 
          iou_multi_translate, participants_avant_translate, participants_apres_translate
      FROM supp
      WHERE iou_multi_translate >= 0.95
  ) sub
  JOIN max_iou mi
  ON sub.idu = mi.idu AND sub.iou_multi_translate = mi.max_iou
  ORDER BY sub.idu, sub.iou_multi_translate DESC;")
  
  dbExecute(conn, "
  INSERT INTO contour_redecoupage
  SELECT supp.idu, supp.nom_com, supp.code_com, supp.com_abs, 
      supp.contenance, mtr.iou_multi, 
      mtr.participants_avant, mtr.participants_apres, 
      supp.geometry
  FROM multi_rapide mtr
  LEFT JOIN supp ON mtr.idu = supp.idu;")
  
  dbExecute(conn, "
  DELETE FROM ajout
  WHERE idu IN (
      SELECT unnest(regexp_split_to_array(participants_apres, ',\\s*')) 
      FROM multi_rapide
  );")
  
  dbExecute(conn, "
  DELETE FROM supp
  WHERE idu IN (
      SELECT unnest(regexp_split_to_array(participants_avant, ',\\s*')) 
      FROM multi_rapide
  );")
  
  dbExecute(conn, "TRUNCATE TABLE multi_calcul_cache;")
  
  dbExecute(conn, "
  WITH updated_values AS (
    SELECT idu,
           (calcul_iou_multi_translate(idu, geometry, 'supp', 'ajout')).*
    FROM supp
  )
  UPDATE supp
  SET iou_multi_translate = updated_values.iou_multi_translate,
      participants_avant_translate = updated_values.participants_avant_translate,
      participants_apres_translate = updated_values.participants_apres_translate
  FROM updated_values
  WHERE supp.idu = updated_values.idu")
  
  dbExecute(conn, "
  INSERT INTO contour_redecoupage
  SELECT idu, nom_com, code_com, com_abs, contenance, iou_multi_translate, 
      participants_avant_translate, participants_apres_translate, geometry
  FROM supp
  WHERE iou_multi_translate >= 0.95;")
  
  dbExecute(conn, "
  DELETE FROM ajout
  WHERE idu IN (
      SELECT unnest(regexp_split_to_array(participants_apres, ',\\s*')) 
      FROM contour_redecoupage
  );")
  
  dbExecute(conn, "
  DELETE FROM supp
  WHERE idu IN (
      SELECT unnest(regexp_split_to_array(participants_avant, ',\\s*')) 
      FROM contour_redecoupage
  );")
  
  dbExecute(conn, "TRUNCATE TABLE multi_calcul_cache;")
  
  dbExecute(conn, "
  INSERT INTO multi_ajout
  WITH calc_results AS (
      SELECT
          idu,
          (calcul_iou_multi(idu, geometry, 'ajout', 'supp')).*
      FROM
          ajout
  )
  SELECT DISTINCT
      unnest(string_to_array(participants_apres, ', ')) AS idu, 
      iou_multi,
      participants_apres,
      participants_avant
  FROM
      calc_results
  WHERE iou_multi >= 0.95;")
  
  dbExecute(conn, "
  INSERT INTO redecoupage
  SELECT supp.idu, supp.nom_com, supp.code_com, supp.com_abs, supp.contenance, 
      ma.iou_multi, ma.participants_avant, ma.participants_apres, supp.geometry
  FROM supp
  LEFT JOIN multi_ajout ma ON ma.idu = supp.idu
  WHERE ma.iou_multi >= 0.99 
      AND LENGTH(ma.participants_apres) != LENGTH(ma.participants_avant);")
  
  dbExecute(conn, "
  INSERT INTO contour
  SELECT supp.idu, supp.nom_com, supp.code_com, supp.com_abs, supp.contenance, 
      ma.iou_multi, ma.participants_avant, ma.participants_apres, supp.geometry
  FROM supp
  LEFT JOIN multi_ajout ma ON ma.idu = supp.idu
  WHERE ma.iou_multi >= 0.95 
      AND LENGTH(ma.participants_apres) = LENGTH(ma.participants_avant);")
  
  dbExecute(conn, "
  INSERT INTO contour_redecoupage
  SELECT supp.idu, supp.nom_com, supp.code_com, supp.com_abs, supp.contenance, 
      ma.iou_multi, ma.participants_avant, ma.participants_apres, supp.geometry
  FROM supp
  LEFT JOIN multi_ajout ma ON ma.idu = supp.idu
  WHERE (ma.iou_multi >= 0.95 
      AND LENGTH(ma.participants_apres) != LENGTH(ma.participants_avant) 
      AND ma.iou_multi < 0.99);")
  
  dbExecute(conn, "
  DELETE FROM ajout
  WHERE idu IN (
      SELECT unnest(regexp_split_to_array(participants_apres, ',\\s*')) 
      FROM redecoupage
  ) OR idu IN (
      SELECT unnest(regexp_split_to_array(participants_apres, ',\\s*')) 
      FROM contour
  ) OR idu IN (
      SELECT unnest(regexp_split_to_array(participants_apres, ',\\s*')) 
      FROM contour_redecoupage
  );")
  
  dbExecute(conn, "
  DELETE FROM supp
  WHERE idu IN (
      SELECT unnest(regexp_split_to_array(participants_avant, ',\\s*')) 
      FROM redecoupage
  ) OR idu IN (
      SELECT unnest(regexp_split_to_array(participants_avant, ',\\s*'))
      FROM contour
  ) OR idu IN (
      SELECT unnest(regexp_split_to_array(participants_avant, ',\\s*')) 
      FROM contour_redecoupage
  );")
  
  dbExecute(conn, "
  INSERT INTO vrai_ajout
  SELECT idu, nom_com, code_com, com_abs, contenance, geometry
  FROM ajout
  WHERE NOT EXISTS (
      SELECT 1 FROM supp WHERE ST_DWithin(ajout.geometry, supp.geometry, 50) 
  );")
  
  dbExecute(conn, "
  INSERT INTO vrai_supp
  SELECT idu, nom_com, code_com, com_abs, contenance, geometry
  FROM supp
  WHERE NOT EXISTS (
      SELECT 1 FROM ajout WHERE ST_DWithin(ajout.geometry, supp.geometry, 50) 
  );")
  
  dbExecute(conn, "
  DELETE FROM ajout
  WHERE EXISTS (
      SELECT 1
      FROM vrai_ajout
      WHERE ajout.idu = vrai_ajout.idu
  );")
  
  dbExecute(conn, "
  DELETE FROM supp
  WHERE EXISTS (
      SELECT 1
      FROM vrai_supp
      WHERE supp.idu = vrai_supp.idu
  );")
  
  dbExecute(conn, "TRUNCATE TABLE multi_calcul_cache;")
  
  dbExecute(conn, paste0("
  INSERT INTO echange_parc
  SELECT idu_translate, avant.nom_com, SUBSTRING(idu_translate FROM 3 FOR 3), 
      tr.idu, tr.nom_com, tr.code_com
  FROM translation tr
  JOIN parc_", params$num_departement, "_", params$temps_avant, " avant
  ON avant.idu = tr.idu_translate
  WHERE SUBSTRING(idu_translate FROM 3 FOR 3) <> tr.code_com
  AND avant.nom_com NOT IN (
      SELECT unnest(regexp_split_to_array(participants, ',\\s*')) 
      FROM chgt_com
      WHERE changement != 'Changement de nom'
  );"))
  
  dbExecute(conn, "
  DELETE FROM translation
  WHERE EXISTS (
      SELECT 1
      FROM echange_parc
      WHERE translation.idu = echange_parc.idu_apres
  );")
  
  
  dbExecute(conn, paste0("
  INSERT INTO echange_parc
  WITH echange AS (
      SELECT DISTINCT 
          (echange_parcelles('participants_apres', 'redecoupage')).*
  )
  SELECT DISTINCT red.participants_avant, ech.nom_com_avant, ech.code_com_avant, 
      red.participants_apres, apres.nom_com, ech.code_com_apres
  FROM echange ech
  JOIN redecoupage red ON red.idu = ech.idu_avant
  JOIN parc_", params$num_departement, "_", params$temps_apres, " apres 
  ON apres.code_com = ech.code_com_apres
  WHERE ech.nom_com_avant NOT IN (
      SELECT unnest(regexp_split_to_array(participants, ',\\s*')) 
      FROM chgt_com
      WHERE changement != 'Changement de nom'
  );")) 
  
  dbExecute(conn, "
  DELETE FROM redecoupage
  WHERE idu IN (
      SELECT unnest(regexp_split_to_array(idu_avant, ',\\s*')) 
      FROM echange_parc
  );")
  
  dbExecute(conn, paste0("
  INSERT INTO echange_parc
  WITH echange AS (
      SELECT DISTINCT 
          (echange_parcelles('participants_apres', 'contour_redecoupage')).*
  )
  SELECT DISTINCT con.participants_avant, ech.nom_com_avant, ech.code_com_avant, 
      con.participants_apres, apres.nom_com, ech.code_com_apres
  FROM echange ech
  JOIN contour_redecoupage con ON con.idu = ech.idu_avant
  JOIN parc_", params$num_departement, "_", params$temps_avant, " apres 
  ON apres.code_com = ech.code_com_apres
  WHERE ech.nom_com_avant NOT IN (
      SELECT unnest(regexp_split_to_array(participants, ',\\s*')) 
      FROM chgt_com
      WHERE changement != 'Changement de nom'
  );"))
  
  dbExecute(conn, "
  DELETE FROM contour_redecoupage
  WHERE idu IN (
      SELECT unnest(regexp_split_to_array(idu_avant, ',\\s*')) 
      FROM echange_parc
  );")
  
  dbExecute(conn, "
  WITH updated_values AS (
    SELECT idu,
           (calcul_iou_multi(idu, geometry, 'supp', 'ajout', 0)).*
    FROM supp
  )
  UPDATE supp
  SET iou_multi = updated_values.iou_multi,
      participants_avant = updated_values.participants_avant,
      participants_apres = updated_values.participants_apres
  FROM updated_values
  WHERE supp.idu = updated_values.idu")
  
  dbExecute(conn, paste0("
  INSERT INTO echange_parc_possible
  WITH echange AS (
      SELECT DISTINCT 
          (echange_parcelles('participants_apres', 'supp')).*
  )
  SELECT DISTINCT supp.participants_avant, ech.nom_com_avant, ech.code_com_avant, 
      supp.participants_apres, apres.nom_com, ech.code_com_apres
  FROM echange ech
  JOIN supp ON supp.idu = ech.idu_avant
  JOIN parc_", params$num_departement, "_", params$temps_avant, " apres 
  ON apres.code_com = ech.code_com_apres
  WHERE ech.nom_com_avant NOT IN (
      SELECT unnest(regexp_split_to_array(participants, ',\\s*')) 
      FROM chgt_com
      WHERE changement != 'Changement de nom'
  );")) 
  
  dbExecute(conn, "
  DELETE FROM ajout
  WHERE idu IN (
      SELECT unnest(regexp_split_to_array(idu_apres, ',\\s*')) 
      FROM echange_parc_possible
  );")
  
  dbExecute(conn, "
  DELETE FROM supp
  WHERE idu IN (
      SELECT unnest(regexp_split_to_array(idu_avant, ',\\s*')) 
      FROM echange_parc_possible
  );")
  
  dbExecute(conn, "
  DROP TABLE IF EXISTS multi_calcul_cache, identique, modif, ajout_simp, 
  supp_simp, identique_bis, fusion_ajout, max_iou, disparition_com, multi_rapide, 
  multi_ajout CASCADE
;")
  
}