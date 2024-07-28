# Créer le schéma temporaire cadastre_temp
dbExecute(conn, paste0(
  "CREATE SCHEMA IF NOT EXISTS traitement_", params$temps_apres, "_", params$temps_avant, "_cadastre_" , params$num_departement, ";"))

# Changer le search_path pour inclure le schéma temporaire
dbExecute(conn, paste0(
  "SET search_path TO traitement_", params$temps_apres, "_", params$temps_avant, "_cadastre_" , params$num_departement,
  ", cadastre_", params$num_departement, ", public"))

dbExecute(conn, "
  CREATE TABLE multi_calcul_cache (
      participants_avant text,
      participants_apres text,
      iou_multi numeric,
      participants_avant_hash text PRIMARY KEY
  );
")

dbExecute(conn, "
  CREATE TABLE ajout (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_ajout_geometry ON ajout USING GIST(geometry);")

dbExecute(conn, "
  CREATE TABLE supp (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_supp_geometry ON supp USING GIST(geometry);")

dbExecute(conn, "
  CREATE TABLE bordure (
      code_insee text PRIMARY KEY,
      nom_com text,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_bordure_geometry ON bordure USING GIST(geometry);")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_bordure_nom_com ON bordure (nom_com);")

dbExecute(conn, "
  CREATE TABLE identique (
      idu text PRIMARY KEY
  );
")

dbExecute(conn, "
  CREATE TABLE modif_avant (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_modif_avant_geometry ON modif_avant USING GIST(geometry);")

dbExecute(conn, "
  CREATE TABLE modif_apres (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_modif_apres_geometry ON modif_apres USING GIST(geometry);")

dbExecute(conn, "
  CREATE TABLE modif (
      idu text PRIMARY KEY,
      geometry_avant geometry(multipolygon, 2154),
      geometry_apres geometry(multipolygon, 2154),
      iou numeric,
      iou_ajust numeric
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_modif_geometry_avant ON modif USING GIST(geometry_avant);")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_modif_geometry_apres ON modif USING GIST(geometry_apres);")

dbExecute(conn, "
  CREATE TABLE modif_avant_iou (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      iou numeric,
      iou_ajust numeric,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_modif_avant_iou_geometry ON modif_avant_iou USING GIST(geometry);")

dbExecute(conn, "
  CREATE TABLE modif_apres_iou (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      iou numeric,
      iou_ajust numeric,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_modif_apres_iou_geometry ON modif_apres_iou USING GIST(geometry);")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_modif_apres_nom_com ON modif_apres (nom_com);")

dbExecute(conn, "
  CREATE TABLE modif_avant_iou_multi (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      iou numeric,
      iou_ajust numeric,
      iou_multi numeric,
      participants_avant text,
      participants_apres text,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_modif_avant_iou_multi_geometry ON modif_avant_iou_multi USING GIST(geometry);")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_modif_avant_iou_multi_nom_com ON modif_avant_iou_multi (nom_com);")

dbExecute(conn, "
  CREATE TABLE ajout_simp (
      idu text PRIMARY KEY,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_ajout_simp_geometry ON ajout_simp USING GIST(geometry);")

dbExecute(conn, "
  CREATE TABLE supp_simp (
      idu text PRIMARY KEY,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_supp_simp_geometry ON supp_simp USING GIST(geometry);")

dbExecute(conn, "
  CREATE TABLE supp_iou (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      iou numeric,
      participants text,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_supp_iou_geometry ON supp_iou USING GIST(geometry);")

dbExecute(conn, "
  CREATE TABLE ajout_iou (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      iou numeric,
      participants text,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_ajout_iou_geometry ON ajout_iou USING GIST(geometry);")

dbExecute(conn, "
  CREATE TABLE ajout_iou_translate (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      iou numeric,
      participants text,
      iou_ajust numeric,
      idu_translate text,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_ajout_iou_translate_geometry ON ajout_iou_translate USING GIST(geometry);")

dbExecute(conn, "
  CREATE TABLE supp_iou_multi (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      iou numeric,
      participants text,
      iou_multi numeric,
      participants_avant text,
      participants_apres text,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_supp_iou_multi_geometry ON supp_iou_multi USING GIST(geometry);")

dbExecute(conn, "
  CREATE TABLE supp_iou_multi_translate_rapide (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      iou numeric,
      participants text,
      iou_multi numeric,
      participants_avant text,
      participants_apres text,
      iou_multi_translate numeric,
      participants_avant_translate text,
      participants_apres_translate text,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_supp_iou_multi_translate_rapide_geometry ON supp_iou_multi_translate_rapide USING GIST(geometry);")

dbExecute(conn, "
  CREATE TABLE max_iou (
      idu text PRIMARY KEY,
      max_iou numeric
  );
")

dbExecute(conn, "
  CREATE TABLE multi_translate_rapide (
      idu text PRIMARY KEY,
      iou_multi_translate numeric,
      participants_avant_translate text,
      participants_apres_translate text
  );
")

dbExecute(conn, "
  CREATE TABLE supp_iou_multi_translate (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      iou numeric,
      participants text,
      iou_multi numeric,
      participants_avant text,
      participants_apres text,
      iou_multi_translate numeric,
      participants_avant_translate text,
      participants_apres_translate text,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_supp_iou_multi_translate_geometry ON supp_iou_multi_translate USING GIST(geometry);")

dbExecute(conn, "
  CREATE TABLE multi_ajout (
      idu text PRIMARY KEY,
      iou_multi numeric,
      participants_avant text,
      participants_apres text
  );
")

dbExecute(conn, "
  CREATE TABLE supp_iou_restant (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      iou numeric,
      participants text,
      iou_multi numeric,
      participants_avant text,
      participants_apres text,
      iou_multi_translate numeric,
      participants_avant_translate text,
      participants_apres_translate text,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_supp_iou_restant_nom_com ON supp_iou_restant (nom_com);")

dbExecute(conn, "
  CREATE TABLE ajout_iou_restant (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      iou numeric,
      participants text,
      iou_ajust numeric,
      idu_translate text,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_ajout_iou_restant_nom_com ON ajout_iou_restant (nom_com);")

# Tables de typologie

# Apres
dbExecute(conn, "
  CREATE TABLE translation (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      iou_ajust numeric,
      idu_translate text,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_translation_nom_com ON translation (nom_com);")

# Avant
dbExecute(conn, "
  CREATE TABLE contour (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      iou_multi numeric,
      participants_avant text,
      participants_apres text,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_contour_nom_com ON contour (nom_com);")

# Avant
dbExecute(conn, "
  CREATE TABLE contour_translation (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      iou_ajust numeric,
      idu_translate text,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_contour_translation_nom_com ON contour_translation (nom_com);")

dbExecute(conn, "
  CREATE TABLE cas_disparition_commune (
      nom_com text PRIMARY KEY,
      code_com text,
      nom_com_avant text,
      code_com_avant text
  );
")

dbExecute(conn, "
  CREATE TABLE chgt_commune (
      nom_com text PRIMARY KEY,
      code_com text,
      changement text,
      participants text,
      participants_code_com text
  );
")

# Apres
dbExecute(conn, "
  CREATE TABLE fusion_com (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      idu_avant text,
      nom_com_avant text,
      code_com_avant text,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_fusion_com_nom_com ON fusion_com (nom_com);")

# Apres
dbExecute(conn, "
  CREATE TABLE defusion_com (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      idu_avant text,
      nom_com_avant text,
      code_com_avant text,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_defusion_com_nom_com ON defusion_com (nom_com);")

# Avant
dbExecute(conn, "
  CREATE TABLE subdiv (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      iou numeric,
      participants text,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_subdiv_nom_com ON subdiv (nom_com);")

# Apres
dbExecute(conn, "
  CREATE TABLE fusion (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      iou numeric,
      participants text,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_fusion_nom_com ON fusion (nom_com);")

# Avant
dbExecute(conn, "
  CREATE TABLE redecoupage (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      iou_multi numeric,
      participants_avant text,
      participants_apres text,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_redecoupage_nom_com ON redecoupage (nom_com);")

# Avant
dbExecute(conn, "
  CREATE TABLE contour_transfo (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      iou_multi numeric,
      participants_avant text,
      participants_apres text,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_contour_transfo_nom_com ON contour_transfo (nom_com);")

# Avant
dbExecute(conn, "
  CREATE TABLE contour_transfo_translation (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      iou_multi_translate numeric,
      participants_avant_translate text,
      participants_apres_translate text,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_contour_transfo_translation_nom_com ON contour_transfo_translation (nom_com);")

# Avant
dbExecute(conn, "
  CREATE TABLE vrai_supp (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      iou numeric,
      participants text,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_vrai_supp_nom_com ON vrai_supp (nom_com);")

# Apres
dbExecute(conn, "
  CREATE TABLE vrai_ajout (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      iou numeric,
      participants text,
      geometry geometry(multipolygon, 2154)
  );
")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_vrai_ajout_nom_com ON vrai_ajout (nom_com);")