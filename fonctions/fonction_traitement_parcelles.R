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
  );"))
  # Parcelles à la période T non présentes à T-1
  
  dbExecute(conn, paste0("
  INSERT INTO supp (idu, nom_com, code_com, com_abs, contenance, geometry)
  SELECT idu, nom_com, code_com, com_abs, contenance, geometry
  FROM parc_", params$num_departement, "_", params$temps_avant, " avant
  WHERE NOT EXISTS (
    SELECT 1
    FROM parc_", params$num_departement, "_", params$temps_apres," apres
    WHERE avant.idu = apres.idu
  );"))
  # Parcelles à la période T-1 non présentes à T
  
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
      com_", params$num_departement, ";"))
  
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
  INSERT INTO modif (
      idu, 
      nom_com, 
      code_com, 
      com_abs, 
      contenance_apres,
      contenance_avant,
      iou,
      geometry_apres,
      geometry_avant)
  SELECT
      apres.idu, apres.nom_com, apres.code_com, apres.com_abs,
      apres.contenance, avant.contenance,
      calcul_iou(apres.geometry, avant.geometry),
      avant.geometry AS geometry_avant, apres.geometry AS geometry_apres
  FROM parc_", params$num_departement, "_", params$temps_apres, " apres
  JOIN parc_", params$num_departement, "_", params$temps_avant, " avant
  ON avant.idu = apres.idu
  WHERE NOT EXISTS (
      SELECT 1
      FROM identique
      WHERE apres.idu = identique.idu_apres
  ) AND NOT EXISTS (
      SELECT 1
      FROM supp
      WHERE avant.idu = supp.idu
  ) AND NOT EXISTS (
      SELECT 1
      FROM ajout
      WHERE apres.idu = ajout.idu
  );"))
    
    dbExecute(conn, "
  DELETE FROM modif
  WHERE iou >= 0.99 OR iou IS NULL;")
    # Elimine les parcelles "sans changement manifeste" plus les parcelles sans iou
    # = parcelles n'ayant pas de géométrie à T ou T-1
    
    dbExecute(conn, "
  UPDATE modif
  SET iou_recale = calcul_iou_recale(m.geometry_apres, m.geometry_avant)
  FROM modif AS m
  WHERE modif.idu = m.idu;")
    # On ne calcule pas directement avant pour éviter des calculs intutiles
    
    dbExecute(conn, "
  INSERT INTO translation
  SELECT idu, nom_com, code_com, com_abs, contenance_apres, iou_recale, 
      idu AS idu_recale, geometry_apres
  FROM modif
  WHERE iou_recale >= 0.99;")
    # Si après translation, une parcelle partage quasiment la même surface, on considètre
    # qu'il s'agit d'une translation.
    # Ps : on a éliminé les concordances parfaites auparavant pour qu'elles ne soient pas
    # considérées à tord comme des translations
    
    dbExecute(conn, "
  INSERT INTO contour
  SELECT idu, nom_com, code_com, com_abs, contenance_avant,
         iou AS iou_multi, idu AS participants_avant,
         idu AS participants_apres, geometry_avant
  FROM modif
  WHERE (iou >= 0.95 OR iou_recale >= 0.95) AND iou_recale < 0.99;")
    # Si avec translation ou non, la parcelle partage quasiment la même surface,
    # il s'agit probablement juste d'une modification de contour
    
    dbExecute(conn, "
  DELETE FROM modif
  WHERE EXISTS (
      SELECT 1
      FROM translation
      WHERE modif.idu = translation.idu
  ) OR EXISTS (
      SELECT 1
      FROM contour
      WHERE modif.idu = contour.idu
  );")
    # On supprime les cas assignés
    
    dbExecute(conn, "
  WITH updated_values AS (
    SELECT idu,
           (calcul_iou_multi_safe(idu, geometry_avant, 'modif')).*
    FROM modif
  )
  UPDATE modif
  SET iou_multi = updated_values.iou_multi,
      participants_avant = updated_values.participants_avant,
      participants_apres = updated_values.participants_apres
  FROM updated_values
  WHERE modif.idu = updated_values.idu")
    # Ce code nous permet juste de savoir probablement les parcelles qui sont
    # impliquées dans les modification de contours restantes.
    
    dbExecute(conn, "
  INSERT INTO contour
  SELECT idu, nom_com, code_com, com_abs, contenance_avant,
         iou_multi, participants_avant, participants_apres, geometry_avant
  FROM modif;")
    # Il n'y a priori pas d'autre cas possibles donc je mets toutes les parcelles restantes
    # dans cette catégorie. Toutefois, je laisse les appels au table modif_avant et modif_après
    # si jamais le code venait à être changer pour laisser des parcelles avec indétermination de cas
    
    dbExecute(conn, "
  DELETE FROM modif
  WHERE idu IN (
      SELECT unnest(regexp_split_to_array(participants_avant, ',\\s*')) 
      FROM contour
  );")
  }
  
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
      FROM parc_", params$num_departement, "_", params$temps_apres,
                         ") OR ajout.nom_com IN (
      SELECT nom_com_apres
      FROM disparition_com GROUP BY nom_com_apres HAVING COUNT(nom_com_avant) > 1
  );"))
  
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
  
  dbExecute(conn, paste0(" 
  UPDATE chgt_com
  SET 
      nom_com_apres = disparition_com.nom_com_apres || ', ' || chgt_com.nom_com_apres,
      code_com_apres = disparition_com.code_com_apres || ', ' || chgt_com.code_com_apres
  FROM disparition_com
  WHERE chgt_com.nom_com_avant = disparition_com.nom_com_avant;"))
  
  
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
      code_com_avant = disparition_com.code_com_avant || ', ' || chgt_com.code_com_avant
  FROM disparition_com
  WHERE chgt_com.nom_com_apres = disparition_com.nom_com_apres;"))
  
  dbExecute(conn, paste0(" 
  INSERT INTO chgt_com
  SELECT nom_com_apres, code_com_apres, 'Changement de nom', nom_com_avant, code_com_avant
  FROM disparition_com
  WHERE nom_com_apres NOT IN (
      SELECT unnest(regexp_split_to_array(nom_com_apres, ',\\s*'))
      FROM chgt_com
  );"))
  
  dbExecute(conn, "
  INSERT INTO ajout_simp
  SELECT idu, nom_com, code_com, ST_SnapToGrid(geometry, 0.0001) AS geometry
  FROM ajout;")
  
  dbExecute(conn, "
  INSERT INTO supp_simp
  SELECT idu, nom_com, code_com, ST_SnapToGrid(geometry, 0.0001) AS geometry
  FROM supp;")
  
  dbExecute(conn, "
  INSERT INTO identique
  SELECT 
      ajout.idu, ajout.nom_com, ajout.code_com,
      supp.idu, supp.nom_com, supp.code_com
  FROM ajout_simp ajout
  JOIN supp_simp  supp
    ON ST_Equals(ajout.geometry, supp.geometry)
  WHERE ST_IsValid(ajout.geometry) 
      AND ST_IsValid(supp.geometry);")
  
  dbExecute(conn, paste0("
  INSERT INTO echange_parc
  WITH avant AS (
      SELECT nom_com_avant, unnest(regexp_split_to_array(nom_com_avant, ',\\s*')) AS nom_avant
      FROM chgt_com
  ),
  apres AS (
      SELECT nom_com_avant, unnest(regexp_split_to_array(nom_com_apres, ',\\s*')) AS nom_apres
      FROM chgt_com
  ),
  couples AS (
    SELECT a.nom_avant, b.nom_apres
    FROM avant a
    JOIN apres b ON a.nom_com_avant = b.nom_com_avant
  )
  SELECT idu_avant, nom_com_avant, code_com_avant, 
      idu_apres, nom_com_apres, code_com_apres  
  FROM identique
  LEFT JOIN couples c ON c.nom_avant = nom_com_avant AND c.nom_apres = nom_com_apres
  WHERE code_com_avant != code_com_apres
      AND (nom_avant IS NULL OR nom_apres IS NULL)
;"))
  
  # Parcelles ajoutées n'ayant pas été modifiées
  dbExecute(conn, "
  DELETE FROM ajout
  WHERE EXISTS (
      SELECT 1
      FROM identique
      WHERE ajout.idu = identique.idu_apres
  );")
  
  # Parcelles supprimées n'ayant pas été modifiées
  dbExecute(conn, "
  DELETE FROM supp
  WHERE EXISTS (
      SELECT 1
      FROM identique
      WHERE supp.idu = identique.idu_avant
  );")
  
  dbExecute(conn, "
  WITH updated_values AS (
    SELECT idu,
           (calcul_iou_intersect(geometry, 'ajout')).*
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
  
  dbExecute(conn, paste0("
  INSERT INTO echange_parc
  WITH avant AS (
      SELECT nom_com_avant, unnest(regexp_split_to_array(nom_com_avant, ',\\s*')) AS nom_avant
      FROM chgt_com
  ),
  apres AS (
      SELECT nom_com_avant, unnest(regexp_split_to_array(nom_com_apres, ',\\s*')) AS nom_apres
      FROM chgt_com
  ),
  couples AS (
    SELECT a.nom_avant, b.nom_apres
    FROM avant a
    JOIN apres b ON a.nom_com_avant = b.nom_com_avant
  )
  SELECT supp.idu, supp.nom_com, supp.code_com, participants, 
      apres.nom_com, SUBSTRING(participants FROM 3 FOR 3) 
  FROM supp
  JOIN parc_", params$num_departement, "_", params$temps_avant, " apres
  ON apres.idu = supp.participants
  LEFT JOIN couples c ON c.nom_avant = supp.nom_com AND c.nom_apres = apres.nom_com
  WHERE iou >= 0.99 AND LENGTH(participants) = 14
  AND SUBSTRING(participants FROM 3 FOR 3) <> supp.code_com 
  AND (nom_avant IS NULL OR nom_apres IS NULL);"))
  
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
           (calcul_iou_intersect(geometry, 'supp')).*
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
      idu,
      nom_com,
      code_com
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
  
  dbExecute(conn, paste0("
  INSERT INTO echange_parc
  WITH avant AS (
      SELECT nom_com_avant, unnest(regexp_split_to_array(nom_com_avant, ',\\s*')) AS nom_avant
      FROM chgt_com
  ),
  apres AS (
      SELECT nom_com_avant, unnest(regexp_split_to_array(nom_com_apres, ',\\s*')) AS nom_apres
      FROM chgt_com
  ),
  couples AS (
    SELECT a.nom_avant, b.nom_apres
    FROM avant a
    JOIN apres b ON a.nom_com_avant = b.nom_com_avant
  )
  SELECT fa.idu, supp.nom_com, supp.code_com, fa.participants_apres, 
      fa.nom_com_apres, fa.code_com_apres
  FROM supp
  JOIN fusion_ajout fa ON fa.idu = supp.idu
  LEFT JOIN couples c ON c.nom_avant = supp.nom_com AND c.nom_apres = fa.nom_com_apres
  WHERE LENGTH(fa.participants_avant) = 14
  AND SUBSTRING(fa.participants_avant FROM 3 FOR 3) <> fa.code_com_apres
  AND (nom_avant IS NULL OR nom_apres IS NULL);"))
  
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
           (calcul_iou_intersect_best_recale(geometry, 'supp')).*
    FROM ajout
  )
  UPDATE ajout
  SET iou_recale = updated_values.iou_recale,
      idu_recale = updated_values.idu_recale
  FROM updated_values
  WHERE ajout.idu = updated_values.idu")
  
  dbExecute(conn, "
  INSERT INTO translation
  SELECT idu, nom_com, code_com, com_abs, contenance, iou_recale, 
      idu_recale, geometry
  FROM ajout
  WHERE iou_recale >= 0.99;")
  
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
      WHERE supp.idu = translation.idu_recale
  );")
  
  dbExecute(conn, "
  WITH updated_values AS (
    SELECT idu,
           (calcul_iou_multi_rapide(geometry, 'supp', 'ajout', false, 0.9, 1)).*
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
  WITH decomposed AS (
      SELECT
          idu,
          unnest(regexp_split_to_array(participants_avant, ',\\s*')) AS participant_id
      FROM multi_rapide
  ),
  participant_groups AS (
      SELECT
          idu,
          array_agg(DISTINCT participant_id ORDER BY participant_id) AS participants_list
      FROM decomposed
      GROUP BY idu
  ),
  conflicting AS (
      SELECT DISTINCT t2.idu
      FROM participant_groups t1
      JOIN participant_groups t2
      ON t1.idu <> t2.idu
      AND t1.idu = ANY(t2.participants_list)
      WHERE t1.participants_list <> t2.participants_list
  )
  DELETE FROM multi_rapide
  WHERE idu  IN (SELECT idu FROM conflicting);
")
  
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
           (calcul_iou_multi_rapide(geometry, 'supp', 'ajout', true, 0, 1)).*
    FROM supp
  )
  UPDATE supp
  SET iou_multi_recale = updated_values.iou_multi,
      participants_avant_recale = updated_values.participants_avant,
      participants_apres_recale = updated_values.participants_apres
  FROM updated_values
  WHERE supp.idu = updated_values.idu")
  
  dbExecute(conn, "
  INSERT INTO max_iou
  SELECT idu, MAX(iou_multi_recale) AS max_iou
  FROM (
      SELECT unnest(regexp_split_to_array(participants_avant_recale, ',\\s*')) AS idu, 
          iou_multi_recale
      FROM supp
      WHERE iou_multi_recale >= 0.95
  ) subquery
  GROUP BY idu;")
  
  dbExecute(conn, "
  INSERT INTO multi_rapide
  SELECT DISTINCT ON (sub.idu) sub.idu, sub.iou_multi_recale, 
      sub.participants_avant_recale, sub.participants_apres_recale
  FROM (
      SELECT unnest(regexp_split_to_array(participants_avant_recale, ',\\s*')) AS idu, 
          iou_multi_recale, participants_avant_recale, participants_apres_recale
      FROM supp
      WHERE iou_multi_recale >= 0.95
  ) sub
  JOIN max_iou mi
  ON sub.idu = mi.idu AND sub.iou_multi_recale = mi.max_iou
  ORDER BY sub.idu, sub.iou_multi_recale DESC;")
  
  dbExecute(conn, "
  WITH decomposed AS (
      SELECT
          idu,
          unnest(regexp_split_to_array(participants_avant, ',\\s*')) AS participant_id
      FROM multi_rapide
  ),
  participant_groups AS (
      SELECT
          idu,
          array_agg(DISTINCT participant_id ORDER BY participant_id) AS participants_list
      FROM decomposed
      GROUP BY idu
  ),
  conflicting AS (
      SELECT DISTINCT t2.idu
      FROM participant_groups t1
      JOIN participant_groups t2
      ON t1.idu <> t2.idu
      AND t1.idu = ANY(t2.participants_list)
      WHERE t1.participants_list <> t2.participants_list
  )
  DELETE FROM multi_rapide
  WHERE idu  IN (SELECT idu FROM conflicting);
")
  
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
           (calcul_iou_multi(idu, geometry, 'supp', 'ajout', true, 0)).*
    FROM supp
  )
  UPDATE supp
  SET iou_multi_recale = updated_values.iou_multi,
      participants_avant_recale = updated_values.participants_avant,
      participants_apres_recale = updated_values.participants_apres
  FROM updated_values
  WHERE supp.idu = updated_values.idu")
  
  dbExecute(conn, "
  INSERT INTO contour_redecoupage
  SELECT idu, nom_com, code_com, com_abs, contenance, iou_multi_recale, 
      participants_avant_recale, participants_apres_recale, geometry
  FROM supp
  WHERE iou_multi_recale >= 0.95;")
  
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
  WITH avant AS (
      SELECT nom_com_avant, unnest(regexp_split_to_array(nom_com_avant, ',\\s*')) AS nom_avant
      FROM chgt_com
  ),
  apres AS (
      SELECT nom_com_avant, unnest(regexp_split_to_array(nom_com_apres, ',\\s*')) AS nom_apres
      FROM chgt_com
  ),
  couples AS (
    SELECT a.nom_avant, b.nom_apres
    FROM avant a
    JOIN apres b ON a.nom_com_avant = b.nom_com_avant
  )
  SELECT idu_recale, avant.nom_com, SUBSTRING(idu_recale FROM 3 FOR 3), 
      tr.idu, tr.nom_com, tr.code_com
  FROM translation tr
  JOIN parc_", params$num_departement, "_", params$temps_avant, " avant
  ON avant.idu = tr.idu_recale
  LEFT JOIN couples c ON c.nom_avant = avant.nom_com AND c.nom_apres = tr.nom_com
  WHERE SUBSTRING(idu_recale FROM 3 FOR 3) <> tr.code_com
  AND (nom_avant IS NULL OR nom_apres IS NULL);"))
  
  dbExecute(conn, "
  DELETE FROM translation
  WHERE EXISTS (
      SELECT 1
      FROM echange_parc
      WHERE translation.idu = echange_parc.participants_apres
  );")
  
  dbExecute(conn, paste0("
  INSERT INTO echange_parc
  WITH avant AS (
      SELECT nom_com_avant, unnest(regexp_split_to_array(nom_com_avant, ',\\s*')) AS nom_avant
      FROM chgt_com
  ),
  apres AS (
      SELECT nom_com_avant, unnest(regexp_split_to_array(nom_com_apres, ',\\s*')) AS nom_apres
      FROM chgt_com
  ),
  couples AS (
      SELECT a.nom_avant, b.nom_apres
      FROM avant a
      JOIN apres b ON a.nom_com_avant = b.nom_com_avant
  ),
  echange AS (
      SELECT 
          (echange_parcelles(
          idu,
          'redecoupage', 
          'parc_", params$num_departement, "_", params$temps_avant, "',
          'parc_", params$num_departement, "_", params$temps_apres, "')).*
      FROM redecoupage
  ),
  resultats_base AS (
      SELECT DISTINCT ON (red.participants_avant) 
          red.participants_avant, 
          red.nom_com AS nom_com_avant, 
          red.code_com AS code_com_avant, 
          red.participants_apres, 
          apres.nom_com AS nom_com_apres, 
          ech.code_com_apres
      FROM echange ech
      JOIN redecoupage red ON red.idu = ech.idu
      JOIN parc_", params$num_departement, "_", params$temps_apres, " apres 
      ON apres.code_com = ech.code_com_apres
  )
  SELECT 
      rb.participants_avant, 
      rb.nom_com_avant, 
      rb.code_com_avant, 
      rb.participants_apres, 
      rb.nom_com_apres, 
      rb.code_com_apres
  FROM resultats_base rb
  LEFT JOIN couples c ON c.nom_avant = rb.nom_com_avant AND c.nom_apres = rb.nom_com_apres
  WHERE c.nom_avant IS NULL OR c.nom_apres IS NULL;
"))
  
  dbExecute(conn, "
  DELETE FROM redecoupage
  WHERE idu IN (
      SELECT unnest(regexp_split_to_array(participants_avant, ',\\s*')) 
      FROM echange_parc
  );")
  
  dbExecute(conn, paste0("
  INSERT INTO echange_parc
  WITH avant AS (
      SELECT nom_com_avant, unnest(regexp_split_to_array(nom_com_avant, ',\\s*')) AS nom_avant
      FROM chgt_com
  ),
  apres AS (
      SELECT nom_com_avant, unnest(regexp_split_to_array(nom_com_apres, ',\\s*')) AS nom_apres
      FROM chgt_com
  ),
  couples AS (
      SELECT a.nom_avant, b.nom_apres
      FROM avant a
      JOIN apres b ON a.nom_com_avant = b.nom_com_avant
  ),
  echange AS (
      SELECT 
          (echange_parcelles(
          idu,
          'contour_redecoupage', 
          'parc_", params$num_departement, "_", params$temps_avant, "',
          'parc_", params$num_departement, "_", params$temps_apres, "')).*
      FROM contour_redecoupage
  ),
  resultats_base AS (
      SELECT DISTINCT ON (con.participants_avant) 
          con.participants_avant, 
          con.nom_com AS nom_com_avant, 
          con.code_com AS code_com_avant, 
          con.participants_apres, 
          apres.nom_com AS nom_com_apres, 
          ech.code_com_apres
      FROM echange ech
      JOIN contour_redecoupage con ON con.idu = ech.idu
      JOIN parc_", params$num_departement, "_", params$temps_apres, " apres 
      ON apres.code_com = ech.code_com_apres
  )
  SELECT 
      rb.participants_avant, 
      rb.nom_com_avant, 
      rb.code_com_avant, 
      rb.participants_apres, 
      rb.nom_com_apres, 
      rb.code_com_apres
  FROM resultats_base rb
  LEFT JOIN couples c ON c.nom_avant = rb.nom_com_avant AND c.nom_apres = rb.nom_com_apres
  WHERE c.nom_avant IS NULL OR c.nom_apres IS NULL;
"))
  
  dbExecute(conn, "
  DELETE FROM contour_redecoupage
  WHERE idu IN (
      SELECT unnest(regexp_split_to_array(participants_avant, ',\\s*')) 
      FROM echange_parc
  );")
  
  dbExecute(conn, "
  WITH updated_values AS (
    SELECT idu,
           (calcul_iou_multi(idu, geometry, 'supp', 'ajout', false, 0)).*
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
  WITH avant AS (
      SELECT nom_com_avant, unnest(regexp_split_to_array(nom_com_avant, ',\\s*')) AS nom_avant
      FROM chgt_com
  ),
  apres AS (
      SELECT nom_com_avant, unnest(regexp_split_to_array(nom_com_apres, ',\\s*')) AS nom_apres
      FROM chgt_com
  ),
  couples AS (
      SELECT a.nom_avant, b.nom_apres
      FROM avant a
      JOIN apres b ON a.nom_com_avant = b.nom_com_avant
  ),
  echange AS (
      SELECT 
          (echange_parcelles(
          idu,
          'supp', 
          'parc_", params$num_departement, "_", params$temps_avant, "',
          'parc_", params$num_departement, "_", params$temps_apres, "')).*
      FROM supp
  ),
  resultats_base AS (
      SELECT DISTINCT ON (supp.participants_avant) 
          supp.participants_avant, 
          supp.nom_com AS nom_com_avant, 
          supp.code_com AS code_com_avant, 
          supp.participants_apres, 
          apres.nom_com AS nom_com_apres, 
          ech.code_com_apres
      FROM echange ech
      JOIN supp ON supp.idu = ech.idu
      JOIN parc_", params$num_departement, "_", params$temps_apres, " apres 
      ON apres.code_com = ech.code_com_apres
  )
  SELECT 
      rb.participants_avant, 
      rb.nom_com_avant, 
      rb.code_com_avant, 
      rb.participants_apres, 
      rb.nom_com_apres, 
      rb.code_com_apres
  FROM resultats_base rb
  LEFT JOIN couples c ON c.nom_avant = rb.nom_com_avant AND c.nom_apres = rb.nom_com_apres
  WHERE c.nom_avant IS NULL OR c.nom_apres IS NULL;
"))
  
  dbExecute(conn, "
  DELETE FROM ajout
  WHERE idu IN (
      SELECT unnest(regexp_split_to_array(participants_apres, ',\\s*')) 
      FROM echange_parc_possible
  );")
  
  dbExecute(conn, "
  DELETE FROM supp
  WHERE idu IN (
      SELECT unnest(regexp_split_to_array(participants_avant, ',\\s*')) 
      FROM echange_parc_possible
  );")
  
  dbExecute(conn, "
  DROP TABLE IF EXISTS multi_calcul_cache, identique, ajout_simp, 
  supp_simp, fusion_ajout, max_iou, disparition_com, multi_rapide, 
  multi_ajout, scission_com, fusion_com CASCADE
;")
  
}