library(plotly)
library(DBI)
rm(list=ls())
source(file = "database/connexion_db.R")
source(file = "fonctions/fonction_shiny.R")
conn <- connecter()

sanke_recap <- function(conn, num_departement, temps_apres, temps_avant, nom_com) {
  final_df <- dbGetQuery(conn, paste0("
      WITH parc_avant AS (
        SELECT nom_com, COUNT(*) AS nb_parcelles_temps_avant
        FROM parc_", num_departement, "_", temps_avant, "
         WHERE nom_com = '", nom_com, "'
         GROUP BY nom_com
      ),
      parc_apres AS (
        SELECT nom_com, COUNT(*) AS nb_parcelles_temps_apres
        FROM parc_", num_departement, "_", temps_apres, "
        WHERE nom_com = '", nom_com, "'
        GROUP BY nom_com
      ),
      modif_restant_avant AS (
        SELECT nom_com, COUNT(*) AS nb_modif_restant_avant
        FROM modif_avant
        WHERE nom_com = '", nom_com, "'
        GROUP BY nom_com
      ),
      supp_restant AS (
        SELECT nom_com, COUNT(*) AS nb_supp_restant
        FROM supp
        WHERE nom_com = '", nom_com, "'
        GROUP BY nom_com
      ),
      modif_restant_apres AS (
        SELECT nom_com, COUNT(*) AS nb_modif_restant_apres
        FROM modif_apres
        WHERE nom_com = '", nom_com, "'
        GROUP BY nom_com
      ),
      ajout_restant AS (
        SELECT nom_com, COUNT(*) AS nb_ajout_restant
        FROM ajout
        WHERE nom_com = '", nom_com, "'
        GROUP BY nom_com
      ),
      vrai_ajout AS (
        SELECT nom_com, COUNT(*) AS nb_ajout
        FROM vrai_ajout
        WHERE nom_com = '", nom_com, "'
        GROUP BY nom_com
      ),
      vrai_supp AS (
        SELECT nom_com, COUNT(*) AS nb_supp
        FROM vrai_supp
        WHERE nom_com = '", nom_com, "'
        GROUP BY nom_com
      ),
      translation AS (
        SELECT nom_com, COUNT(*) AS nb_translation
        FROM translation
        WHERE nom_com = '", nom_com, "'
        GROUP BY nom_com
      ),
      contour AS (
        SELECT nom_com, COUNT(*) AS nb_contour
        FROM contour
        WHERE nom_com = '", nom_com, "'
        GROUP BY nom_com
      ),
      redecoupage AS (
        SELECT nom_com, COUNT(DISTINCT participants_avant) AS nb_redecoupage
        FROM redecoupage
        WHERE nom_com = '", nom_com, "'
        GROUP BY nom_com
      ),
      contour_redecoupage AS (
        SELECT nom_com, COUNT(DISTINCT participants_avant) AS nb_contour_redecoupage
        FROM contour_redecoupage
        WHERE nom_com = '", nom_com, "'
        GROUP BY nom_com
      ),
      echange_avant AS (
          SELECT nom_com_avant AS nom_com, COUNT(DISTINCT idu) AS nb_echanges
          FROM echange_parc,
          LATERAL unnest(regexp_split_to_array(idu_avant, ',\\s*')) AS idu
          WHERE nom_com_avant = '", nom_com, "'
          GROUP BY nom_com_avant
      ),
      echange_apres AS (
          SELECT nom_com_apres AS nom_com, COUNT(DISTINCT idu) AS nb_echanges
          FROM echange_parc,
          LATERAL unnest(regexp_split_to_array(idu_apres, ',\\s*')) AS idu
          WHERE nom_com_apres = '", nom_com, "'
          GROUP BY nom_com_apres
      ),
      echange AS (
          SELECT nom_com, SUM(nb_echanges) AS nb_echanges
          FROM (
            SELECT * FROM echange_avant
            UNION ALL
            SELECT * FROM echange_apres
        ) AS combined
        GROUP BY nom_com
      ),
      echange_poss_avant AS (
          SELECT nom_com_avant AS nom_com, COUNT(DISTINCT idu) AS nb_echanges_poss
          FROM echange_parc_possible,
          LATERAL unnest(regexp_split_to_array(idu_avant, ',\\s*')) AS idu
          WHERE nom_com_avant = '", nom_com, "'
          GROUP BY nom_com_avant
      ),
      echange_poss_apres AS (
          SELECT nom_com_apres AS nom_com, COUNT(DISTINCT idu) AS nb_echanges_poss
          FROM echange_parc_possible,
          LATERAL unnest(regexp_split_to_array(idu_apres, ',\\s*')) AS idu
          WHERE nom_com_apres = '", nom_com, "'
          GROUP BY nom_com_apres
      ),
      echange_poss AS (
          SELECT nom_com, SUM(nb_echanges_poss) AS nb_echanges_poss
          FROM (
            SELECT * FROM echange_poss_avant
            UNION ALL
            SELECT * FROM echange_poss_apres
        ) AS combined
        GROUP BY nom_com
      ),
      base AS (
        SELECT 
          COALESCE(parc_avant.nom_com, parc_apres.nom_com) AS nom_com,
          COALESCE(nb_parcelles_temps_apres, 0) AS parcelles_20", temps_apres,",
          COALESCE(nb_parcelles_temps_avant, 0) AS parcelles_20", temps_avant,"
        FROM parc_avant
        FULL OUTER JOIN parc_apres ON parc_avant.nom_com = parc_apres.nom_com
      )
      SELECT 
        base.nom_com AS nom, 
        parcelles_20", temps_apres," AS total_20", temps_apres,",
        parcelles_20", temps_avant," AS total_20", temps_avant,",
        COALESCE(nb_modif_restant_apres, 0) + COALESCE(nb_ajout_restant, 0) AS taux_classification_20", temps_apres,",
        COALESCE(nb_modif_restant_avant, 0) + COALESCE(nb_supp_restant, 0) AS taux_classification_20", temps_avant,",
        COALESCE(nb_ajout, 0) AS ajout,
        COALESCE(nb_supp, 0) AS suppression,
        COALESCE(nb_translation, 0) AS translation,
        COALESCE(nb_contour, 0) AS contour,
        COALESCE(nb_redecoupage, 0) AS redécoupage,
        COALESCE(nb_contour_redecoupage, 0) AS contour_redécoupage,
        COALESCE(nb_echanges, 0) AS échange,
        COALESCE(nb_echanges_poss, 0) AS échange_possible
      FROM base
      FULL OUTER JOIN modif_restant_avant ON base.nom_com = modif_restant_avant.nom_com
      FULL OUTER JOIN supp_restant ON base.nom_com = supp_restant.nom_com
      FULL OUTER JOIN modif_restant_apres ON base.nom_com = modif_restant_apres.nom_com
      FULL OUTER JOIN ajout_restant ON base.nom_com = ajout_restant.nom_com
      FULL OUTER JOIN vrai_ajout ON base.nom_com = vrai_ajout.nom_com
      FULL OUTER JOIN vrai_supp ON base.nom_com = vrai_supp.nom_com
      FULL OUTER JOIN translation ON base.nom_com = translation.nom_com
      FULL OUTER JOIN contour ON base.nom_com = contour.nom_com
      FULL OUTER JOIN redecoupage ON base.nom_com = redecoupage.nom_com
      FULL OUTER JOIN contour_redecoupage ON base.nom_com = contour_redecoupage.nom_com
      FULL OUTER JOIN echange ON base.nom_com = echange.nom_com
      FULL OUTER JOIN echange_poss ON base.nom_com = echange_poss.nom_com;"))
  
  final_df <- final_df %>%
    mutate(across(where(bit64::is.integer64), as.integer)) %>% 
    mutate(!!paste0("taux_classification_20", temps_apres) := paste0(
      round((!!sym(paste0("total_20", temps_apres)) - !!sym(paste0("taux_classification_20", temps_apres))) / 
              !!sym(paste0("total_20", temps_apres)) * 100, 3), "% (",!!sym(paste0("taux_classification_20", temps_apres)), ")"),
      !!paste0("taux_classification_20", temps_avant) := paste0(
        round((!!sym(paste0("total_20", temps_avant)) - !!sym(paste0("taux_classification_20", temps_avant))) / 
                !!sym(paste0("total_20", temps_avant)) * 100, 3), "% (",!!sym(paste0("taux_classification_20", temps_avant)),")"))
    
}

maj_chemin <- function(conn, num_departement, temps_apres, temps_avant) {
  dbExecute(conn, paste0(
    "SET search_path TO traitement_", temps_apres, "_", temps_avant, "_cadastre_" , num_departement, 
    ", cadastre_", num_departement, ", public"
  ))
  print(paste0("MAJ chemin: période de temps: 20", temps_apres, " - 20", temps_avant, 
               ", département: ", num_departement))
}
maj_chemin(conn, "85", "24", "23")
données <- sanke_recap(conn, "85", "24", "23", "Saint-Avaugourd-des-Landes")

données["translation"]

fig <- plot_ly(
  type = "sankey",
  orientation = "h",
  valueformat = ".0f",
  
  node = list(
    label = c("Parcelles à T-1", #0
              "Identique", #1
              "Translation", #2
              "Contour", #3
              "Fusion", #4
              "Scission", #5
              "Redécoupage", #6 
              "Non-classé", #7
              "Echange", #8
              "Echange possible", #9
              "Suppression", #10
              "Ajout", #11
              "Parcelles à T" #12
              ),
    color = c("#286AC7", "white", "#FFDA5A", "#069F9C", "#FF9999", "#CC99FF", 
              "#AE48C0", "black", "#520408", "#78080F", "#E91422", "#26A44B", "#FFC300"),
    pad = 5,
    thickness = 30,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  11),
    target = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12),
    value =  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1)
  )
)

fig
