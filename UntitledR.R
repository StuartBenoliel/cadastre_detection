dbExecute(conn, paste0("
  INSERT INTO identique
  SELECT 
      apres.idu, apres.nom_com, apres.code_com,
      avant.idu, avant.nom_com, avant.code_com
  FROM parc_", params$num_departement, "_", params$temps_apres, " apres
  JOIN parc_", params$num_departement, "_", params$temps_avant, " avant 
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
# On récupèrer les parcelles identiques pour lesquelles on a arrondi au préalable à 
# 10^-4 les cordonnées de la géomètrie (vraiment utile parfois). Toutefois, lors
# de l'arrondissage, certains polygones peuvent s'auto-intersecter ce qui génére une erreur.
# Si ce cas la arrive, la parcelle n'est pas considéré comme identique.

dbExecute(conn, paste0(" 
  INSERT INTO disparition_com
  SELECT DISTINCT
      nom_com_apres,
      code_com_apres,
      nom_com_avant,
      code_com_avant
  FROM identique 
  WHERE nom_com_apres != nom_com_avant;"))
# Changement de nom (ou du à une fusion)


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
  JOIN avant ON ajout.idu = avant.idu_apres;")
# On tente de reformer l'identifiant 


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
      FROM parc_", params$num_departement, "_", params$temps_apres, ");"))

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
      FROM disparition_com GROUP BY nom_com_avant HAVING COUNT(nom_com_apres) = 1);"))

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
      df.participants,
      df.participants_code_com,
      CASE 
          WHEN c.nom_com IS NOT NULL THEN 
              'Scission partielle'
          ELSE 
              'Scission totale'
      END AS changement,
      df.nom_com_avant,
      df.code_com_avant
  FROM
      scission_data df
  LEFT JOIN 
      com_apres c ON c.nom_com = df.nom_com_avant;"))



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
  JOIN supp ON apres.idu_avant = supp.idu;")

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
      FROM disparition_com GROUP BY nom_com_avant HAVING COUNT(nom_com_apres) = 1);"))

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
      f.nom_com, c.nom_com, f.code_com, f.participants, f.participants_code_com;"))

dbExecute(conn, paste0(" 
  UPDATE chgt_com
  SET 
      nom_com_avant = disparition_com.nom_com_avant || ', ' || chgt_com.nom_com_avant,
      code_com_avant = disparition_com.code_com_avant || ', ' || chgt_com.nom_com_avant
  FROM disparition_com
  WHERE chgt_com.nom_com_apres = disparition_com.nom_com_apres;"))

dbExecute(conn, paste0(" 
  INSERT INTO chgt_com
  SELECT nom_com_apres, code_com_apres, 'Changement de nom', nom_com_avant, code_com_avant
  FROM disparition_com
  WHERE nom_com_apres NOT IN (SELECT nom_com_apres FROM chgt_com);"))