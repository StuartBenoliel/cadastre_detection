code_sys_projection <- function(num_departement) {
  syst_proj <- c(
    "971" = 5490,
    "972" = 5490,
    "973" = 2972,
    "974" = 2975,
    "976" = 4471,
    "977" = 5490,
    "978" = 5490
  )
  if (as.character(num_departement) %in% names(syst_proj)) {
    return(syst_proj[[as.character(num_departement)]])
  } else {
    return(2154)
  }
}

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
  );")

dbExecute(conn, paste0("
  CREATE TABLE ajout (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      iou numeric,
      participants text,
      iou_recale numeric,
      idu_recale text,
      geometry geometry(multipolygon, ", code_sys_projection(params$num_departement),")
  );"))
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_ajout_geometry ON ajout USING GIST(geometry);")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_ajout_nom_com ON ajout (nom_com);")

dbExecute(conn, paste0("
  CREATE TABLE supp (
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
      iou_multi_recale numeric,
      participants_avant_recale text,
      participants_apres_recale text,
      geometry geometry(multipolygon, ", code_sys_projection(params$num_departement),")
  );"))
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_supp_geometry ON supp USING GIST(geometry);")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_supp_nom_com ON supp (nom_com);")

dbExecute(conn, paste0("
  CREATE TABLE bordure (
      code_insee text PRIMARY KEY,
      nom_com text,
      geometry geometry(multipolygon, ", code_sys_projection(params$num_departement),")
  );
"))
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_bordure_geometry ON bordure USING GIST(geometry);")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_bordure_nom_com ON bordure (nom_com);")

dbExecute(conn, paste0("
  CREATE TABLE identique (
      idu_apres text PRIMARY KEY,
      nom_com_apres text,
      code_com_apres text,
      idu_avant text,
      nom_com_avant text,
      code_com_avant text
  );
"))
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_identique_idu_avant ON identique (idu_avant);")

dbExecute(conn, paste0("
  CREATE TABLE modif (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance_apres numeric,
      contenance_avant numeric,
      iou numeric,
      iou_recale numeric,
      iou_multi numeric,
      participants_avant text,
      participants_apres text,
      geometry_apres geometry(multipolygon, ", code_sys_projection(params$num_departement),"),
      geometry_avant geometry(multipolygon, ", code_sys_projection(params$num_departement),")
  );
"))
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_modif_nom_com ON modif (nom_com);")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_modif_geometry_avant ON modif USING GIST(geometry_avant);")
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_modif_geometry_apres ON modif USING GIST(geometry_apres);")

dbExecute(conn, "
  CREATE TABLE disparition_com (
      nom_com_apres text PRIMARY KEY,
      code_com_apres text,
      nom_com_avant text,
      code_com_avant text
  );
")

dbExecute(conn, paste0("
  CREATE TABLE ajout_simp (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      geometry geometry(multipolygon, ", code_sys_projection(params$num_departement),")
  );
"))
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_ajout_simp_geometry ON ajout_simp USING GIST(geometry);")

dbExecute(conn, paste0("
  CREATE TABLE supp_simp (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      geometry geometry(multipolygon, ", code_sys_projection(params$num_departement),")
  );
"))
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_supp_simp_geometry ON supp_simp USING GIST(geometry);")

dbExecute(conn, "
  CREATE TABLE fusion_ajout (
      idu text PRIMARY KEY,
      iou numeric,
      participants_avant text,
      participants_apres text,
      nom_com_apres text,
      code_com_apres text
  );
")

dbExecute(conn, "
  CREATE TABLE max_iou (
      idu text PRIMARY KEY,
      max_iou numeric
  );
")

dbExecute(conn, "
  CREATE TABLE multi_rapide (
      idu text PRIMARY KEY,
      iou_multi numeric,
      participants_avant text,
      participants_apres text
  );
")


dbExecute(conn, "
  CREATE TABLE multi_ajout (
      idu text PRIMARY KEY,
      iou_multi numeric,
      participants_avant text,
      participants_apres text
  );
")


# Tables de typologie

# Apres
dbExecute(conn, paste0("
  CREATE TABLE translation (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      iou_recale numeric,
      idu_recale text,
      geometry geometry(multipolygon, ", code_sys_projection(params$num_departement),")
  );
"))
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_translation_nom_com ON translation (nom_com);")

# Avant
dbExecute(conn, paste0("
  CREATE TABLE contour (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      iou_multi numeric,
      participants_avant text,
      participants_apres text,
      geometry geometry(multipolygon, ", code_sys_projection(params$num_departement),")
  );
"))
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_contour_nom_com ON contour (nom_com);")

dbExecute(conn, "
  CREATE TABLE chgt_com (
      nom_com_apres text PRIMARY KEY,
      code_com_apres text,
      changement text,
      nom_com_avant text,
      code_com_avant text
  );
")

# Apres
dbExecute(conn, paste0("
  CREATE TABLE fusion_com (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      idu_avant text,
      nom_com_avant text,
      code_com_avant text,
      geometry geometry(multipolygon, ", code_sys_projection(params$num_departement),")
  );
"))
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_fusion_com_nom_com ON fusion_com (nom_com);")

# Apres
dbExecute(conn, paste0("
  CREATE TABLE scission_com (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      idu_avant text,
      nom_com_avant text,
      code_com_avant text,
      geometry geometry(multipolygon, ", code_sys_projection(params$num_departement),")
  );
"))
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_scission_com_nom_com ON scission_com (nom_com);")

# Avant
dbExecute(conn, paste0("
  CREATE TABLE redecoupage (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      iou_multi numeric,
      participants_avant text,
      participants_apres text,
      geometry geometry(multipolygon, ", code_sys_projection(params$num_departement),")
  );
"))
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_redecoupage_nom_com ON redecoupage (nom_com);")

# Avant
dbExecute(conn, paste0("
  CREATE TABLE contour_redecoupage (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      iou_multi numeric,
      participants_avant text,
      participants_apres text,
      geometry geometry(multipolygon, ", code_sys_projection(params$num_departement),")
  );
"))
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_contour_redecoupage_nom_com ON contour_redecoupage (nom_com);")

# Avant
dbExecute(conn, paste0("
  CREATE TABLE vrai_supp (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      geometry geometry(multipolygon, ", code_sys_projection(params$num_departement),")
  );
"))
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_vrai_supp_nom_com ON vrai_supp (nom_com);")

# Apres
dbExecute(conn, paste0("
  CREATE TABLE vrai_ajout (
      idu text PRIMARY KEY,
      nom_com text,
      code_com text,
      com_abs text,
      contenance numeric,
      geometry geometry(multipolygon, ", code_sys_projection(params$num_departement),")
  );
"))
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_vrai_ajout_nom_com ON vrai_ajout (nom_com);")

# Apres
dbExecute(conn, paste0("
  CREATE TABLE echange_parc (
      participants_avant text PRIMARY KEY,
      nom_com_avant text,
      code_com_avant text,
      participants_apres text,
      nom_com_apres text,
      code_com_apres text
  );
"))
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_echange_parc_nom_com_avant ON echange_parc (nom_com_avant);")

dbExecute(conn, paste0("
  CREATE TABLE echange_parc_possible (
      participants_avant text PRIMARY KEY,
      nom_com_avant text,
      code_com_avant text,
      participants_apres text,
      nom_com_apres text,
      code_com_apres text
  );
"))
dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_echange_parc_possible_nom_com_avant ON echange_parc_possible (nom_com_avant);")