departement_traite <- function(conn) {
  result <- dbGetQuery(conn, paste0(" 
  SELECT 
      schema_name
  FROM 
      information_schema.schemata
  WHERE 
      schema_name LIKE 'traitement_%_%_cadastre_%';
           "))
  
  departements <- result$schema_name %>%
    str_extract("cadastre_.*\\z") %>%
    str_replace_all("cadastre_", "") %>%
    unique()  %>%                                     # Obtenir des valeurs uniques
    tibble(value = .) %>%                            # Convertir en tibble pour faciliter le tri
    mutate(
      numeric_part = as.numeric(str_extract(value, "\\d+")),    # Extraire la partie numérique
      non_numeric_part = str_remove(value, "\\d+")              # Extraire la partie non numérique
    ) %>%
    arrange(numeric_part, non_numeric_part) %>%         # Trier d'abord par partie numérique, puis par partie non numérique
    pull(value)   
}

reconnect_db <- function() {
  tryCatch({
    conn <- connecter()
    return(conn)
  }, error = function(e) {
    message("Erreur de connexion à la base de données : ", e$message)
    NULL
  })
}

keep_alive <- function(conn) {
  invalidateLater(60000)  # 60000 millisecondes = 1 minute
  tryCatch({
    DBI::dbGetQuery(conn, "SELECT 1")
  }, error = function(e) {
    message("Erreur de connexion, tentative de reconnexion : ", e$message)
    conn <<- reconnect_db()  # Tenter de se reconnecter
  })
}

# Fonction pour mettre à jour le search path
maj_chemin <- function(conn, num_departement, temps_apres, temps_avant) {
  dbExecute(conn, paste0(
    "SET search_path TO traitement_", temps_apres, "_", temps_avant, "_cadastre_" , num_departement, 
    ", cadastre_", num_departement, ", public"
  ))
  #print(paste0("MAJ chemin: période de temps: 20", temps_apres, " - 20", temps_avant, ", département: ", num_departement))
}

intervalle_temps <- function(conn, num_departement){
  result <- dbGetQuery(conn, paste0(" 
      SELECT 
          schema_name
      FROM 
          information_schema.schemata
      WHERE 
          schema_name LIKE 'traitement_%_%_cadastre_", num_departement,"';
               "))
  
  int_temps <- result$schema_name %>%
    str_extract("traitement_(\\d{2})_(\\d{2})_cadastre") %>%
    str_replace_all("traitement_(\\d{2})_(\\d{2})_cadastre", "\\1-\\2") %>%
    as.character()
  
  int_temps <- int_temps[order(as.numeric(str_extract(int_temps, "^[0-9]+")) , decreasing = TRUE)]
}

nom_code_commune <- function(conn, num_departement, temps_apres){
  commune <- dbGetQuery(conn, paste0(
    "SELECT DISTINCT nom_com, code_com FROM parc_", num_departement, "_", temps_apres, ";"))
  commune <- sort(paste(commune$nom_com, commune$code_com))
}

cartes_dynamiques <- function(conn, num_departement, temps_apres, temps_avant, nom_com) {
  
  bordure <- st_read(conn, query = paste0(
    "SELECT * FROM bordure WHERE nom_com IN (", nom_com, ");"))
  parc_avant <- st_read(conn, query = paste0(
    "SELECT * FROM parc_", num_departement, "_", temps_avant, " WHERE nom_com IN (", nom_com, ");"))
  parc_apres <- st_read(conn, query = paste0(
    "SELECT * FROM parc_", num_departement, "_", temps_apres, " WHERE nom_com IN (", nom_com, ");"))
  
  modif_apres <- st_read(conn, query =  paste0(
    "SELECT idu, nom_com, code_com, com_abs, contenance_apres,
        iou, iou_recale, iou_multi, participants_avant, 
        participants_apres, geometry_apres 
    FROM modif WHERE nom_com IN (", nom_com, ");"))
  
  ajout <- st_read(conn, query =  paste0(
    "SELECT * FROM ajout WHERE nom_com IN (", nom_com, ");"))
  supp <- st_read(conn, query =  paste0(
    "SELECT * FROM supp WHERE nom_com IN (", nom_com, ");"))
  
  translation <- st_read(conn, query = paste0(
    "SELECT * FROM translation WHERE nom_com IN (", nom_com, ");"))
  vrai_ajout <- st_read(conn, query = paste0(
    "SELECT * FROM vrai_ajout WHERE nom_com IN (", nom_com, ");"))
  vrai_supp <- st_read(conn, query = paste0(
    "SELECT * FROM vrai_supp WHERE nom_com IN (", nom_com, ");"))
  
  echange_parc <- dbGetQuery(conn, paste0(
    "SELECT * FROM echange_parc WHERE nom_com_avant IN (", nom_com, ") OR nom_com_apres IN (", nom_com, ");"))
  echange_parc_possible <- dbGetQuery(conn, paste0(
    "SELECT * FROM echange_parc_possible WHERE nom_com_avant IN (", nom_com, ") OR nom_com_apres IN (", nom_com, ");"))
  
  is_fusion_or_nom_com <- dbGetQuery(conn, paste0(
    "SELECT * FROM chgt_com WHERE nom_com_apres IN (", nom_com, ") AND (changement = 'Fusion' OR changement = 'Changement de nom');"))
  is_scission_part_com <- dbGetQuery(conn, paste0(
    "SELECT * FROM chgt_com WHERE nom_com_avant IN (", nom_com, ") AND changement = 'Scission partielle' ;"))
  is_scission_com <- dbGetQuery(conn, paste0(
    "SELECT * FROM chgt_com WHERE regexp_split_to_array(nom_com_apres, ',\\s*') && ARRAY[", 
    nom_com, "] AND changement != 'Fusion';"))
  
  if (nrow(bordure) > 0) {
    
    contour_commune <- st_read(conn, query = paste0(
      "SELECT nom_com, code_insee, ST_Boundary(geometry) AS geometry FROM com_", num_departement, " WHERE nom_com IN (", nom_com, ");"))
    
    map_base <- mapview(bordure, 
                        layer.name = "Bordures étendues", col.regions = "#F2F2F2", 
                        alpha.regions = 0.7, homebutton = F,
                        map.types = c("CartoDB.Positron", "OpenStreetMap", "Esri.WorldImagery")) +
      mapview(contour_commune, color = "black",
              layer.name = "Contour communal",
              homebutton = F,
              legend = FALSE,
              map.types = c("CartoDB.Positron", "OpenStreetMap", "Esri.WorldImagery"))
    
    
    map_base_2 <- mapview(contour_commune, legend = FALSE, color = "black",
                          layer.name = "Contour communal",
                          homebutton = F, 
                          map.types = c("CartoDB.Positron", "OpenStreetMap", "Esri.WorldImagery"))
  } else {
    map_base <- mapview(map.types = c("CartoDB.Positron", "OpenStreetMap", "Esri.WorldImagery"))
    
    map_base_2 <- map_base
  }
  
  map_1 <- map_base
  
  if (nrow(is_fusion_or_nom_com) > 0) {
    
    parc_avant <- st_read(conn, query = paste0(
      "SELECT * FROM parc_", num_departement, "_", temps_avant, 
      " WHERE nom_com IN (", nom_com, ")
           OR nom_com IN 
        (SELECT unnest(regexp_split_to_array(nom_com_avant, ',\\s*')) 
          FROM chgt_com WHERE nom_com_apres IN (", nom_com, "));"))
    
    modif_avant <- st_read(conn, query =  paste0(
      "SELECT idu, nom_com, code_com, com_abs, contenance_avant,
          iou, iou_recale, iou_multi, participants_avant, 
          participants_apres, geometry_avant 
      FROM modif WHERE nom_com IN (", nom_com, ")
           OR nom_com IN 
        (SELECT unnest(regexp_split_to_array(nom_com_avant, ',\\s*')) 
          FROM chgt_com WHERE nom_com_apres IN (", nom_com, "));"))
    
    supp <- st_read(conn, query =  paste0(
      "SELECT * FROM supp WHERE nom_com IN (", nom_com, ")
           OR nom_com IN 
        (SELECT unnest(regexp_split_to_array(nom_com_avant, ',\\s*')) 
          FROM chgt_com WHERE nom_com_apres IN (", nom_com, "));"))
    
    vrai_supp <- st_read(conn, query = paste0(
      "SELECT * FROM vrai_supp WHERE nom_com IN (", nom_com, ")
           OR nom_com IN 
        (SELECT unnest(regexp_split_to_array(nom_com_avant, ',\\s*')) 
          FROM chgt_com WHERE nom_com_apres IN (", nom_com, "));"))
    
    contour <- st_read(conn, query = paste0(
      "SELECT * FROM contour WHERE nom_com IN (", nom_com, ")
           OR nom_com IN 
        (SELECT unnest(regexp_split_to_array(nom_com_avant, ',\\s*')) 
          FROM chgt_com WHERE nom_com_apres IN (", nom_com, "));"))
    
    redecoupage <- st_read(conn, query = paste0(
      "SELECT * FROM redecoupage WHERE nom_com IN (", nom_com, ")
           OR nom_com IN 
        (SELECT unnest(regexp_split_to_array(nom_com_avant, ',\\s*')) 
          FROM chgt_com WHERE nom_com_apres IN (", nom_com, "));"))
    
    contour_redecoupage <- st_read(conn, query = paste0(
      "SELECT * FROM contour_redecoupage WHERE nom_com IN (", nom_com, ")
           OR nom_com IN 
        (SELECT unnest(regexp_split_to_array(nom_com_avant, ',\\s*')) 
          FROM chgt_com WHERE nom_com_apres IN (", nom_com, "));"))
    
  } else if (nrow(is_scission_part_com) > 0) {
    
    
    modif_avant <- st_read(conn, query = paste0(
      "SELECT m.idu, m.nom_com, m.code_com, m.com_abs, m.contenance_avant,
            m.iou, m.iou_recale, m.iou_multi, m.participants_avant, 
            m.participants_apres, m.geometry_avant
        FROM modif m
        JOIN parc_", num_departement, "_", temps_apres, " pa ON m.idu = pa.idu
        WHERE pa.nom_com IN (", nom_com, ");"))
    
    contour <- st_read(conn, query = paste0(
      "SELECT co.* 
         FROM contour co
         JOIN parc_", num_departement, "_", temps_apres, " pa ON
           pa.idu = ANY (string_to_array(co.participants_apres, ','))
         WHERE pa.nom_com IN (", nom_com, ");"))
    
    redecoupage <- st_read(conn, query = paste0(
      "SELECT red.* 
         FROM redecoupage red
         JOIN parc_", num_departement, "_", temps_apres, " pa ON
           pa.idu = ANY (string_to_array(red.participants_apres, ','))
         WHERE pa.nom_com IN (", nom_com, ");"))
    
    contour_redecoupage <- st_read(conn, query = paste0(
      "SELECT cot.* 
         FROM contour_redecoupage cot
         JOIN parc_", num_departement, "_", temps_apres, " pa ON
           pa.idu = ANY (string_to_array(cot.participants_apres, ','))
         WHERE pa.nom_com IN (", nom_com, ");"))
    
  } else if (nrow(is_scission_com) > 0) {
    
    parc_avant <- st_read(conn, query = paste0(
      "SELECT * FROM parc_", num_departement, "_", temps_avant, 
      " WHERE nom_com IN (", nom_com, ")
           OR nom_com =
        (SELECT nom_com_avant FROM chgt_com 
          WHERE regexp_split_to_array(nom_com_apres, ',\\s*') && ARRAY[", nom_com, "]);"))
    
    modif_avant <- st_read(conn, query = paste0(
      "SELECT m.idu, m.nom_com, m.code_com, m.com_abs, m.contenance_avant,
            m.iou, m.iou_recale, m.iou_multi, m.participants_avant, 
            m.participants_apres, m.geometry_avant
         FROM modif m
         JOIN parc_", num_departement, "_", temps_apres, " pa ON m.idu = pa.idu
         WHERE m.nom_com = 
              (SELECT nom_com_avant FROM chgt_com 
                WHERE regexp_split_to_array(nom_com_apres, ',\\s*') && ARRAY[", nom_com, "]
           AND pa.nom_com IN (", nom_com, "));"))
    
    supp <- st_read(conn, query =  paste0(
      "SELECT * FROM supp WHERE nom_com IN (", nom_com, ")
           OR nom_com = 
              (SELECT nom_com_avant FROM chgt_com 
                WHERE regexp_split_to_array(nom_com_apres, ',\\s*') && ARRAY[", nom_com, "]);"))
    
    vrai_supp <- st_read(conn, query = paste0(
      "SELECT * FROM vrai_supp WHERE nom_com IN (", nom_com, ")
           OR nom_com = 
              (SELECT nom_com_avant FROM chgt_com 
                WHERE regexp_split_to_array(nom_com_apres, ',\\s*') && ARRAY[", nom_com, "]);"))
    
    contour <- st_read(conn, query = paste0(
      "SELECT co.* 
         FROM contour co
         JOIN parc_", num_departement, "_", temps_apres, " pa ON
           pa.idu = ANY (string_to_array(co.participants_apres, ','))
         WHERE co.nom_com = 
              (SELECT nom_com_avant FROM chgt_com 
                WHERE regexp_split_to_array(nom_com_apres, ',\\s*') && ARRAY[", nom_com, "]
           AND pa.nom_com IN (", nom_com, "));"))
    
    redecoupage <- st_read(conn, query = paste0(
      "SELECT red.* 
         FROM redecoupage red
         JOIN parc_", num_departement, "_", temps_apres, " pa ON
           pa.idu = ANY (string_to_array(red.participants_apres, ','))
         WHERE red.nom_com = 
              (SELECT nom_com_avant FROM chgt_com 
                WHERE regexp_split_to_array(nom_com_apres, ',\\s*') && ARRAY[", nom_com, "]
           AND pa.nom_com IN (", nom_com, "));"))
    
    contour_redecoupage <- st_read(conn, query = paste0(
      "SELECT cot.* 
         FROM contour_redecoupage cot
         JOIN parc_", num_departement, "_", temps_apres, " pa ON
           pa.idu = ANY (string_to_array(cot.participants_apres, ','))
         WHERE cot.nom_com = 
              (SELECT nom_com_avant FROM chgt_com 
                WHERE regexp_split_to_array(nom_com_apres, ',\\s*') && ARRAY[", nom_com, "]
           AND pa.nom_com IN (", nom_com, "));"))
    
  } else {

    modif_avant <- st_read(conn, query =  paste0(
      "SELECT idu, nom_com, code_com, com_abs, contenance_avant,
          iou, iou_recale, iou_multi, participants_avant, 
          participants_apres, geometry_avant
      FROM modif WHERE nom_com IN (", nom_com, ");"))
    contour <- st_read(conn, query = paste0(
      "SELECT * FROM contour WHERE nom_com IN (", nom_com, ");"))
    redecoupage <- st_read(conn, query = paste0(
      "SELECT * FROM redecoupage WHERE nom_com IN (", nom_com, ");"))
    contour_redecoupage <- st_read(conn, query = paste0(
      "SELECT * FROM contour_redecoupage WHERE nom_com IN (", nom_com, ");"))
  }
  
  if (nrow(translation) > 0) {
    
    map_1 <- map_1 + mapview(translation,
                             layer.name = paste0("Parcelles translatées (état 20",temps_apres,")"), 
                             col.regions = "#FFDA5A",
                             alpha.regions = 0.5, homebutton = F) +
      mapview(parc_avant %>%
                filter(idu %in% translation$idu_recale), 
              col.regions = "#FFDA5A",
              layer.name = paste0("Parcelles translatées (état 20",temps_avant,")"), 
              alpha.regions = 0.5, homebutton = F)
    
  }
  if (nrow(contour) > 0) {
    
    map_1 <- map_1 + mapview(parc_apres %>%
                               filter(idu %in% unlist(str_split(contour$participants_apres, ",\\s*"))),  
                             layer.name = paste0("Parcelles contours (état 20",temps_apres,")"), 
                             col.regions = "#069F9C", alpha.regions = 0.5, homebutton = F) +
      mapview(contour,  
              layer.name = paste0("Parcelles contours (état 20",temps_avant,")"),
              col.regions = "#069F9C",
              alpha.regions = 0.5, homebutton = F)
  }
  if (nrow(redecoupage) > 0) {
    
    map_1 <- map_1 + mapview(parc_apres %>%
                               filter(idu %in% unlist(str_split(redecoupage$participants_apres, ",\\s*"))),
                             layer.name = paste0("Parcelles redécoupées (état 20",temps_apres,")"),
                             col.regions = "#AE48C0",
                             alpha.regions = 0.5, homebutton = F) +
      mapview(redecoupage,  
              layer.name = paste0("Parcelles redécoupées (état 20",temps_avant,")"), 
              col.regions = "#AE48C0", alpha.regions = 0.5, homebutton = F)
  }
  if (nrow(contour_redecoupage) > 0) {
    
    map_1 <- map_1 + mapview(parc_apres %>%
                               filter(idu %in% unlist(str_split(contour_redecoupage$participants_apres, ",\\s*"))),  
                             layer.name = paste0("Parcelles redécoupées + contours (état 20",temps_apres,")"),
                             col.regions = "#268DFF",
                             alpha.regions = 0.5, homebutton = F) +
      mapview(contour_redecoupage,
              layer.name = paste0("Parcelles redécoupées + contours (état 20",temps_avant,")"), 
              col.regions = "#268DFF", alpha.regions = 0.5, homebutton = F)
  }
  if (nrow(echange_parc) > 0) {
    
    echange_parc_avant <- st_read(conn, query = paste0(
      "SELECT * FROM parc_", num_departement, "_", temps_avant, " 
      WHERE idu IN (
          SELECT unnest(regexp_split_to_array(participants_avant, ',\\s*')) 
          FROM echange_parc
          WHERE nom_com_avant IN (", nom_com, ") OR nom_com_apres IN (", nom_com, ")
    );"))
    
    echange_parc_apres <- st_read(conn, query = paste0(
      "SELECT * FROM parc_", num_departement, "_", temps_apres, " 
      WHERE idu IN (
          SELECT unnest(regexp_split_to_array(participants_apres, ',\\s*')) 
          FROM echange_parc
          WHERE nom_com_avant IN (", nom_com, ") OR nom_com_apres IN (", nom_com, ")
    );"))
    
    map_1 <- map_1 + mapview(echange_parc_apres,  
                             layer.name = paste0("Parcelles échangées (état 20",temps_apres,")"),
                             col.regions = "#520408",
                             alpha.regions = 0.5, homebutton = F) +
      mapview(echange_parc_avant,
              layer.name = paste0("Parcelles échangées (état 20",temps_avant,")"), 
              col.regions = "#520408", alpha.regions = 0.5, homebutton = F)
  }
  if (nrow(echange_parc_possible) > 0) {
    
    echange_parc_possible_avant <- st_read(conn, query = paste0(
      "SELECT * FROM parc_", num_departement, "_", temps_avant, " 
      WHERE idu IN (
          SELECT unnest(regexp_split_to_array(participants_avant, ',\\s*')) 
          FROM echange_parc_possible
          WHERE nom_com_avant IN (", nom_com, ") OR nom_com_apres IN (", nom_com, ")
    );"))
    
    echange_parc_possible_apres <- st_read(conn, query = paste0(
      "SELECT * FROM parc_", num_departement, "_", temps_apres, " 
      WHERE idu IN (
          SELECT unnest(regexp_split_to_array(participants_apres, ',\\s*')) 
          FROM echange_parc_possible
          WHERE nom_com_avant IN (", nom_com, ") OR nom_com_apres IN (", nom_com, ")
    );"))
    
    map_1 <- map_1 + mapview(echange_parc_possible_apres,  
                             layer.name = paste0("Parcelles possiblement échangées (état 20",temps_apres,")"),
                             col.regions = "#78080F",
                             alpha.regions = 0.5, homebutton = F) +
      mapview(echange_parc_possible_avant,
              layer.name = paste0("Parcelles possiblement échangées (état 20",temps_avant,")"), 
              col.regions = "#78080F", alpha.regions = 0.5, homebutton = F)
  }
  if (nrow(vrai_ajout[!st_is_empty(vrai_ajout), ]) > 0) {
    
    map_1 <- map_1 + mapview(vrai_ajout, 
                             layer.name = "Parcelles ajoutées", 
                             col.regions = "#26A44B", alpha.regions = 0.8, homebutton = F)
  }
  if (nrow(vrai_supp[!st_is_empty(vrai_supp), ]) > 0) {
    
    map_1 <- map_1 + mapview(vrai_supp,
                             layer.name = "Parcelles supprimées",
                             col.regions = "#E91422", alpha.regions = 0.8, homebutton = F) 
  }
  if (nrow(supp) > 0) {
    
    map_1 <- map_1 + mapview(supp, col.regions = "#FFE2E2",
                             layer.name = paste0("Parcelles restantes (état 20",temps_avant,")"), 
                             alpha.regions = 0.5, homebutton = F)
  }
  if (nrow(modif_avant) > 0) {
    
    map_1 <- map_1 + mapview(modif_avant, col.regions = "#FFE2E2",
                             layer.name = paste0("Parcelles restantes (état 20",temps_avant,")"),
                             alpha.regions = 0.5, homebutton = F)
  }
  if (nrow(ajout) > 0) {
    
    map_1 <- map_1 + mapview(ajout, col.regions = "#DAF7E2",
                             layer.name = paste0("Parcelles restantes (état 20",temps_apres,")"), 
                             alpha.regions = 0.5, homebutton = F)
  }
  if (nrow(modif_apres) > 0) {
    
    map_1 <- map_1 + mapview(modif_apres,  col.regions = "#DAF7E2",
                             layer.name = paste0("Parcelles restantes (état 20",temps_apres,")"),
                             alpha.regions = 0.5, homebutton = F)
  }
  
  map_2 <- mapview(parc_apres, 
                   layer.name = paste0("Parcelles (état 20",temps_apres,")"),
                   col.regions = "#286AC7", 
                   homebutton = FALSE) + map_base_2
  
  map_3 <- mapview(parc_avant, 
                   layer.name = paste0("Parcelles (état 20",temps_avant,")"), 
                   col.regions = "#FFC300",
                   homebutton = FALSE) + map_base_2
  
  map_compa <- map_2 | map_3
  
  sync(map_1, map_compa@map, ncol = 1)
}

check_refonte_pc <- function(conn, nom_com, seuil=50) {
  nb_parcelles_rest <- dbGetQuery(conn, paste0("
  SELECT 
    (SELECT COUNT(*) 
     FROM modif
     WHERE nom_com IN (", nom_com, ")) AS nb_modif_restant,
    (SELECT COUNT(*) 
     FROM supp
     WHERE nom_com IN (", nom_com, ")) AS nb_supp_restant,
    (SELECT COUNT(*) 
     FROM modif
     WHERE nom_com IN (", nom_com, ")) +
    (SELECT COUNT(*) 
     FROM supp
     WHERE nom_com IN (", nom_com, ")) AS total_count
"))
  ifelse(nb_parcelles_rest >50, return(T), return(F))
}

tableau_si_donnee <- function(table) {
  tagList(
    if (nrow(table) > 0) {
      datatable(table, options = list(paging = FALSE, searching = FALSE, 
                                      autoWidth = TRUE, ordering = TRUE), rownames = FALSE)
    } else {
      div(
        style = "text-align: center; margin-top: 20px;",  # Centrage du texte et espacement
        h4("Aucun cas à priori")
      )
    }
  )
}

tableau_recap <- function(conn, num_departement, temps_apres, temps_avant, var) {
  final_df <- dbGetQuery(conn, paste0("
      WITH parc_avant AS (
        SELECT nom_com, code_com, COUNT(*) AS nb_parcelles_temps_avant
        FROM parc_", num_departement, "_", temps_avant, "
        GROUP BY code_com, nom_com
      ),
      parc_apres AS (
        SELECT nom_com, code_com, COUNT(*) AS nb_parcelles_temps_apres
        FROM parc_", num_departement, "_", temps_apres, "
        GROUP BY code_com, nom_com
      ),
      modif_restant AS (
        SELECT nom_com, COUNT(*) AS nb_modif_restant
        FROM modif
        GROUP BY nom_com
      ),
      supp_restant AS (
        SELECT nom_com, COUNT(*) AS nb_supp_restant
        FROM supp
        GROUP BY nom_com
      ),
      ajout_restant AS (
        SELECT nom_com, COUNT(*) AS nb_ajout_restant
        FROM ajout
        GROUP BY nom_com
      ),
      vrai_ajout AS (
        SELECT nom_com, COUNT(*) AS nb_ajout
        FROM vrai_ajout
        GROUP BY nom_com
      ),
      vrai_supp AS (
        SELECT nom_com, COUNT(*) AS nb_supp
        FROM vrai_supp
        GROUP BY nom_com
      ),
      translation AS (
        SELECT nom_com, COUNT(*) AS nb_translation
        FROM translation
        GROUP BY nom_com
      ),
      contour AS (
        SELECT nom_com, COUNT(*) AS nb_contour
        FROM contour
        GROUP BY nom_com
      ),
      redecoupage AS (
        SELECT nom_com, COUNT(DISTINCT participants_avant) AS nb_redecoupage
        FROM redecoupage
        GROUP BY nom_com
      ),
      contour_redecoupage AS (
        SELECT nom_com, COUNT(DISTINCT participants_avant) AS nb_contour_redecoupage
        FROM contour_redecoupage
        GROUP BY nom_com
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
          COALESCE(parc_avant.code_com, parc_apres.code_com) AS code_com,
          COALESCE(nb_parcelles_temps_apres, 0) AS parcelles_20", temps_apres,",
          COALESCE(nb_parcelles_temps_avant, 0) AS parcelles_20", temps_avant,"
        FROM parc_avant
        FULL OUTER JOIN parc_apres ON parc_avant.nom_com = parc_apres.nom_com
      )
      SELECT 
        base.nom_com AS nom, 
        code_com AS code,
        parcelles_20", temps_apres," AS total_20", temps_apres,",
        parcelles_20", temps_avant," AS total_20", temps_avant,",
        COALESCE(nb_modif_restant, 0) + COALESCE(nb_ajout_restant, 0) AS taux_classif_20", temps_apres,",
        COALESCE(nb_modif_restant, 0) + COALESCE(nb_supp_restant, 0) AS taux_classif_20", temps_avant,",
        COALESCE(nb_ajout, 0) AS ajout,
        COALESCE(nb_supp, 0) AS suppression,
        COALESCE(nb_translation, 0) AS translation,
        COALESCE(nb_contour, 0) AS contour,
        COALESCE(nb_redecoupage, 0) AS redécoupage,
        COALESCE(nb_contour_redecoupage, 0) AS contour_redécoupage,
        COALESCE(nb_echanges, 0) AS échange,
        COALESCE(nb_echanges_poss, 0) AS échange_possible
      FROM base
      FULL OUTER JOIN modif_restant ON base.nom_com = modif_restant.nom_com
      FULL OUTER JOIN supp_restant ON base.nom_com = supp_restant.nom_com
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
    mutate(!!paste0("taux_classif_20", temps_apres) := 
      round((!!sym(paste0("total_20", temps_apres)) - !!sym(paste0("taux_classif_20", temps_apres))) / 
              !!sym(paste0("total_20", temps_apres)) * 100, 2),
      !!paste0("taux_classif_20", temps_avant) := 
        round((!!sym(paste0("total_20", temps_avant)) - !!sym(paste0("taux_classif_20", temps_avant))) / 
                !!sym(paste0("total_20", temps_avant)) * 100, 2))

  datatable(final_df[, var], 
            options = list(pageLength = 20,
                           ordering = TRUE, 
                           orderClasses = TRUE,
                           rowCallback = DT::JS(
                             'function(row, data) {
                  // Bold cells for those >= 1.0 in all numeric columns
                  for (var i = 0; i < data.length; i++) {
                    var cellValue = parseFloat(data[i]);
                    if (!isNaN(cellValue) && cellValue >= 1.0) {
                      $("td:eq(" + i + ")", row).css("font-weight", "bold");
                    }
                  }
                }')), rownames = FALSE)
}