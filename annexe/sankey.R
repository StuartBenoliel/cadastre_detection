library(plotly)
library(DBI)
rm(list=ls())
source(file = "database/connexion_db.R")
source(file = "fonctions/fonction_shiny.R")
conn <- connecter()

sanke_recap <- function(conn, num_departement, temps_apres, temps_avant) {
  final_df <- dbGetQuery(conn, paste0("
      WITH parc_avant AS (
        SELECT COUNT(*) AS nb_parcelles_temps_avant
        FROM parc_", num_departement, "_", temps_avant, "
      ),
      parc_apres AS (
        SELECT COUNT(*) AS nb_parcelles_temps_apres
        FROM parc_", num_departement, "_", temps_apres, "
      ),
      modif_restant AS (
        SELECT COUNT(*) AS nb_modif_restant
        FROM modif
      ),
      supp_restant AS (
        SELECT COUNT(*) AS nb_supp_restant
        FROM supp
      ),
      ajout_restant AS (
        SELECT COUNT(*) AS nb_ajout_restant
        FROM ajout
      ),
      vrai_ajout AS (
        SELECT COUNT(*) AS nb_ajout
        FROM vrai_ajout
      ),
      vrai_supp AS (
        SELECT COUNT(*) AS nb_supp
        FROM vrai_supp
      ),
      translation AS (
        SELECT COUNT(*) AS nb_translation
        FROM translation
      ),
      contour AS (
        SELECT COUNT(*) AS nb_contour
        FROM contour
      ),
      fusion AS (
        SELECT COUNT(participants_avant) AS nb_fusion_avant,
            COUNT(DISTINCT participants_apres) AS nb_fusion_apres
        FROM redecoupage
        WHERE LENGTH(participants_apres) = 14
      ),
      scission AS (
          SELECT COUNT(DISTINCT participants_avant) AS nb_scission_avant,
                 COUNT(p.participant) AS nb_scission_apres 
          FROM redecoupage
          LEFT JOIN LATERAL (
              SELECT unnest(regexp_split_to_array(participants_apres, ',\\s*')) AS participant
          ) p ON true
          WHERE LENGTH(participants_avant) = 14
      ),
      redecoupage AS (
        SELECT COUNT(DISTINCT idu_avant) AS nb_redecoupage_avant, COUNT(DISTINCT idu_apres) AS nb_redecoupage_apres
        FROM (
            SELECT unnest(regexp_split_to_array(participants_avant, ',\\s*')) AS idu_avant,
                unnest(regexp_split_to_array(participants_apres, ',\\s*')) AS idu_apres
            FROM redecoupage
            WHERE LENGTH(participants_avant) != 14 AND LENGTH(participants_apres) != 14
        ) AS decomposed
      ),
      contour_fusion AS (
        SELECT COUNT(participants_avant) AS nb_contour_fusion_avant,
            COUNT(DISTINCT participants_apres) AS nb_contour_fusion_apres
        FROM contour_redecoupage
        WHERE LENGTH(participants_apres) = 14
      ),
      contour_scission AS (
          SELECT COUNT(DISTINCT participants_avant) AS nb_contour_scission_avant,
                 COUNT(DISTINCT p.participant) AS nb_contour_scission_apres 
          FROM contour_redecoupage
          LEFT JOIN LATERAL (
              SELECT unnest(regexp_split_to_array(participants_apres, ',\\s*')) AS participant
          ) p ON true
          WHERE LENGTH(participants_avant) = 14
      ),
      contour_redecoupage AS (
        SELECT COUNT(DISTINCT idu_avant) AS nb_contour_redecoupage_avant, COUNT(DISTINCT idu_apres) AS nb_contour_redecoupage_apres
        FROM (
            SELECT unnest(regexp_split_to_array(participants_avant, ',\\s*')) AS idu_avant,
                unnest(regexp_split_to_array(participants_apres, ',\\s*')) AS idu_apres
            FROM contour_redecoupage
            WHERE LENGTH(participants_avant) != 14 AND LENGTH(participants_apres) != 14
        ) AS decomposed
      ),
      echange_avant AS (
          SELECT nom_com_avant AS nom_com, COUNT(DISTINCT idu) AS nb_echanges
          FROM echange_parc,
          LATERAL unnest(regexp_split_to_array(participants_avant, ',\\s*')) AS idu
          GROUP BY nom_com_avant
      ),
      echange_apres AS (
          SELECT nom_com_apres AS nom_com, COUNT(DISTINCT idu) AS nb_echanges
          FROM echange_parc,
          LATERAL unnest(regexp_split_to_array(participants_apres, ',\\s*')) AS idu
          GROUP BY nom_com_apres
      ),
      echange AS (
          SELECT SUM(nb_echanges) AS nb_echanges
          FROM (
            SELECT * FROM echange_avant
            UNION ALL
            SELECT * FROM echange_apres
        ) AS combined
      ),
      echange_poss_avant AS (
          SELECT nom_com_avant AS nom_com, COUNT(DISTINCT idu) AS nb_echanges_poss
          FROM echange_parc_possible,
          LATERAL unnest(regexp_split_to_array(participants_avant, ',\\s*')) AS idu
          GROUP BY nom_com_avant
      ),
      echange_poss_apres AS (
          SELECT nom_com_apres AS nom_com, COUNT(DISTINCT idu) AS nb_echanges_poss
          FROM echange_parc_possible,
          LATERAL unnest(regexp_split_to_array(participants_apres, ',\\s*')) AS idu
          GROUP BY nom_com_apres
      ),
      echange_poss AS (
          SELECT SUM(nb_echanges_poss) AS nb_echanges_poss
          FROM (
            SELECT * FROM echange_poss_avant
            UNION ALL
            SELECT * FROM echange_poss_apres
        ) AS combined
      ),
      base AS (
        SELECT 
          COALESCE(nb_parcelles_temps_apres, 0) AS parcelles_20", temps_apres,",
          COALESCE(nb_parcelles_temps_avant, 0) AS parcelles_20", temps_avant,"
        FROM parc_avant,parc_apres
      )
      SELECT 
        parcelles_20", temps_apres," AS total_20", temps_apres,",
        parcelles_20", temps_avant," AS total_20", temps_avant,",
        COALESCE(nb_modif_restant, 0) + COALESCE(nb_ajout_restant, 0) AS restantes_20", temps_apres,",
        COALESCE(nb_modif_restant, 0) + COALESCE(nb_supp_restant, 0) AS restantes_20", temps_avant,",
        COALESCE(nb_ajout, 0) AS ajout,
        COALESCE(nb_supp, 0) AS suppression,
        COALESCE(nb_translation, 0) AS translation,
        COALESCE(nb_contour, 0) AS contour,
        COALESCE(nb_fusion_avant, 0) + COALESCE(nb_contour_fusion_avant, 0) AS fusion_avant,
        COALESCE(nb_fusion_apres, 0) + COALESCE(nb_contour_fusion_apres, 0) AS fusion_apres,
        COALESCE(nb_scission_avant, 0) + COALESCE(nb_contour_scission_avant, 0) AS scission_avant,
        COALESCE(nb_scission_apres, 0) + COALESCE(nb_contour_scission_apres, 0) AS scission_apres,
        COALESCE(nb_redecoupage_avant, 0) + COALESCE(nb_contour_redecoupage_avant, 0) AS redécoupage_avant,
        COALESCE(nb_redecoupage_apres, 0) + COALESCE(nb_contour_redecoupage_apres, 0) AS redécoupage_apres,
        COALESCE(nb_echanges, 0) + COALESCE(nb_echanges_poss, 0) AS échange
      FROM base,
          modif_restant,
          supp_restant,
          ajout_restant,
          vrai_ajout,
          vrai_supp,
          translation,
          contour,
          fusion, 
          scission,
          redecoupage,
          contour_fusion, 
          contour_scission,
          contour_redecoupage,
          echange,
          echange_poss;"))
  
  final_df <- final_df %>%
    mutate(across(where(bit64::is.integer64), as.integer))
    
}
# 11380
# 17953
maj_chemin <- function(conn, num_departement, temps_apres, temps_avant) {
  dbExecute(conn, paste0(
    "SET search_path TO traitement_", temps_apres, "_", temps_avant, "_cadastre_" , num_departement, 
    ", cadastre_", num_departement, ", public"
  ))
  print(paste0("MAJ chemin: période de temps: 20", temps_apres, " - 20", temps_avant, 
               ", département: ", num_departement))
}
maj_chemin(conn, "85", "24", "23")
données <- sanke_recap(conn, "85", "24", "23")

données["translation"]
# 1252513
fig <- plot_ly(
  type = "sankey",
  orientation = "h",
  valueformat = ".0f",
  
  node = list(
    label = c("Parcelles de 2023<br>modifiées : 11380<br>(Total : 1263893)", #0
              "Translation<br>(1044 / 1044)", #1
              "Contour<br>(1942 / 1942)", #2
              "Fusion<br>(1052 / 310)", #3
              "Scission<br>(4189 / 11843)", #4
              "Redécoupage<br>(2187 / 1922)", #5 
              "Non-classé<br>(809 / 641)", #6
              "Echange<br>(0 / 0)", #7
              "Suppression : 157", #8
              "Ajout : 251", #9
              "Parcelles de 2024<br>modifiées : 17953<br>(Total : 1270466)" #10
    ),
    color = c("#286AC7", "#FFDA5A", "#069F9C", "#FF9999", "#CC99FF", 
              "#AE48C0", "white", "#520408", "#E91422", "#26A44B", "#FFC300"),
    pad = 10,
    thickness = 30,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source = c(0, 0, 0, 0, 0, 0, 0, 0,  
               1,  2,  3,  4,  5,  6,  7,  9),
    target = c(1, 2, 3, 4, 5, 6, 7, 8, 
               10, 10, 10, 10, 10, 10, 10, 10),
    value =  c(1044, 1942, 1052, 4189, 2187, 809, 0, 157,  
               1044, 1942, 310, 11843, 1922, 641, 0, 251)
  )
)

fig %>% layout(
  font = list(
    size = 20,
    color = 'black'
  ),
  plot_bgcolor = '#F5F5F5',  # Couleur de fond du graphique
  paper_bgcolor = '#F5F5F5' # Couleur de fond du papier
)

