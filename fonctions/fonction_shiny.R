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

# Fonction pour mettre à jour le search path
maj_chemin <- function(conn, num_departement, temps_apres, temps_avant) {
  dbExecute(conn, paste0(
    "SET search_path TO traitement_", temps_apres, "_", temps_avant, "_cadastre_" , num_departement, 
    ", cadastre_", num_departement, ", public"
  ))
  print(paste0("MAJ chemin: période de temps: 20", temps_apres, " - 20", temps_avant, 
               ", département: ", num_departement))
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
    "SELECT * FROM bordure WHERE nom_com = '", nom_com, "';"))
  parc_avant <- st_read(conn, query = paste0(
    "SELECT * FROM parc_", num_departement, "_", temps_avant, " WHERE nom_com = '", nom_com, "';"))
  parc_apres <- st_read(conn, query = paste0(
    "SELECT * FROM parc_", num_departement, "_", temps_apres, " WHERE nom_com = '", nom_com, "';"))
  
  modif_apres <- st_read(conn, query =  paste0(
    "SELECT * FROM modif_apres WHERE nom_com = '",nom_com, "';"))
  ajout <- st_read(conn, query =  paste0(
    "SELECT * FROM ajout WHERE nom_com = '",nom_com, "';"))
  supp <- st_read(conn, query =  paste0(
    "SELECT * FROM supp WHERE nom_com = '",nom_com, "';"))
  
  translation <- st_read(conn, query = paste0(
    "SELECT * FROM translation WHERE nom_com = '",nom_com, "';"))
  fusion <- st_read(conn, query = paste0(
    "SELECT * FROM fusion WHERE nom_com = '", nom_com, "';"))
  vrai_ajout <- st_read(conn, query = paste0(
    "SELECT * FROM vrai_ajout WHERE nom_com = '", nom_com, "';"))
  vrai_supp <- st_read(conn, query = paste0(
    "SELECT * FROM vrai_supp WHERE nom_com = '", nom_com, "';"))
  
  is_fusion_or_nom__com <- dbGetQuery(conn, paste0(
    "SELECT * FROM chgt_commune WHERE nom_com = '", nom_com, "' AND (changement = 'Fusion' OR changement = 'Changement de nom');"))
  
  is_defusion_part_com  <- dbGetQuery(conn, paste0(
    "SELECT * FROM chgt_commune WHERE nom_com = '", nom_com, "' AND changement = 'Défusion partielle' ;"))
  
  is_defusion_com  <- dbGetQuery(conn, paste0(
    "SELECT * FROM chgt_commune WHERE '", nom_com, "' = ANY(regexp_split_to_array(participants, ',\\s*'))
      AND changement != 'Fusion';"))
  
  if (nrow(bordure) > 0) {
    map_base <- mapview(bordure, 
                        layer.name = "Bordures étendues", col.regions = "#F2F2F2", 
                        alpha.regions = 0.7, homebutton = F, 
                        map.types = c("CartoDB.Positron", "OpenStreetMap", "Esri.WorldImagery"))
  } else {
    map_base <- mapview(map.types = c("CartoDB.Positron", "OpenStreetMap", "Esri.WorldImagery"))
  }
  
  map_1 <- map_base
  
  if (nrow(is_fusion_or_nom__com) > 0) {
    
    fusion_com <- st_read(conn, query = paste0(
      "SELECT * FROM fusion_com WHERE nom_com = '", nom_com, "';"))
    
    fusion_com_avant <- st_read(conn, query = paste0(
      "SELECT * FROM parc_", num_departement, "_", temps_avant, 
      " WHERE idu IN 
            (SELECT idu_avant FROM fusion_com WHERE nom_com = '", nom_com, "');"))
    
    parc_avant <- st_read(conn, query = paste0(
      "SELECT * FROM parc_", num_departement, "_", temps_avant, 
      " WHERE nom_com = '", nom_com, "'
           OR nom_com IN 
        (SELECT unnest(regexp_split_to_array(participants, ',\\s*')) 
          FROM chgt_commune WHERE nom_com = '", nom_com, "');"))
    
    modif_avant <- st_read(conn, query =  paste0(
      "SELECT * FROM modif_avant WHERE nom_com = '", nom_com, "'
           OR nom_com IN 
        (SELECT unnest(regexp_split_to_array(participants, ',\\s*')) 
          FROM chgt_commune WHERE nom_com = '", nom_com, "');"))
    
    supp <- st_read(conn, query =  paste0(
      "SELECT * FROM supp WHERE nom_com = '", nom_com, "'
           OR nom_com IN 
        (SELECT unnest(regexp_split_to_array(participants, ',\\s*')) 
          FROM chgt_commune WHERE nom_com = '", nom_com, "');"))
    
    vrai_supp <- st_read(conn, query = paste0(
      "SELECT * FROM vrai_supp WHERE nom_com = '", nom_com, "'
           OR nom_com IN 
        (SELECT unnest(regexp_split_to_array(participants, ',\\s*')) 
          FROM chgt_commune WHERE nom_com = '", nom_com, "');"))
    
    contour <- st_read(conn, query = paste0(
      "SELECT * FROM contour WHERE nom_com = '", nom_com, "'
           OR nom_com IN 
        (SELECT unnest(regexp_split_to_array(participants, ',\\s*')) 
          FROM chgt_commune WHERE nom_com = '", nom_com, "');"))
    
    contour_translation <- st_read(conn, query = paste0(
      "SELECT * FROM contour_translation WHERE nom_com = '", nom_com, "'
           OR nom_com IN 
        (SELECT unnest(regexp_split_to_array(participants, ',\\s*')) 
          FROM chgt_commune WHERE nom_com = '", nom_com, "');"))
    
    subdiv <- st_read(conn, query = paste0(
      "SELECT * FROM subdiv WHERE nom_com = '", nom_com, "'
           OR nom_com IN 
        (SELECT unnest(regexp_split_to_array(participants, ',\\s*')) 
          FROM chgt_commune WHERE nom_com = '", nom_com, "');"))
    
    redecoupage <- st_read(conn, query = paste0(
      "SELECT * FROM redecoupage WHERE nom_com = '", nom_com, "'
           OR nom_com IN 
        (SELECT unnest(regexp_split_to_array(participants, ',\\s*')) 
          FROM chgt_commune WHERE nom_com = '", nom_com, "');"))
    
    contour_redecoupage <- st_read(conn, query = paste0(
      "SELECT * FROM contour_redecoupage WHERE nom_com = '", nom_com, "'
           OR nom_com IN 
        (SELECT unnest(regexp_split_to_array(participants, ',\\s*')) 
          FROM chgt_commune WHERE nom_com = '", nom_com, "');"))
    
    contour_redecoupage_translation <- st_read(conn, query = paste0(
      "SELECT * FROM contour_redecoupage_translation WHERE nom_com = '", nom_com, "'
           OR nom_com IN 
        (SELECT unnest(regexp_split_to_array(participants, ',\\s*')) 
          FROM chgt_commune WHERE nom_com = '", nom_com, "');"))
    
    map_1 <- map_1 + mapview(fusion_com,  
                             layer.name = paste0("Parcelles fusion de communes (état 20",temps_apres,")"), 
                             col.regions = "white",
                             alpha.regions = 0.5, homebutton = F) +
      mapview(fusion_com_avant,  
              layer.name = paste0("Parcelles fusion de communes (état 20",temps_avant,")"), 
              col.regions = "white", alpha.regions = 0.5, homebutton = F)
    
  } else if (nrow(is_defusion_part_com) > 0) {
    
    defusion_com <- st_read(conn, query = paste0(
      "SELECT * FROM defusion_com WHERE nom_com_avant = '", nom_com, "';"))
    
    defusion_com_avant <- st_read(conn, query = paste0(
      "SELECT * FROM parc_", num_departement, "_", temps_avant, 
      " WHERE idu IN (SELECT idu_avant FROM defusion_com WHERE nom_com_avant = '", nom_com, "');"))
    
    modif_avant <- st_read(conn, query = paste0(
      "SELECT ma.* 
         FROM modif_avant ma
         JOIN parc_", num_departement, "_", temps_apres, " pa ON ma.idu = pa.idu
         WHERE pa.nom_com = '", nom_com, "';"))
    
    contour <- st_read(conn, query = paste0(
      "SELECT co.* 
         FROM contour co
         JOIN parc_", num_departement, "_", temps_apres, " pa ON
           pa.idu = ANY (string_to_array(co.participants_apres, ','))
         WHERE pa.nom_com = '", nom_com, "';"))
    
    contour_translation <- st_read(conn, query = paste0(
      "SELECT cot.* 
         FROM contour_translation cot
         JOIN parc_", num_departement, "_", temps_apres, " pa ON cot.idu_translate = pa.idu
         WHERE pa.nom_com = '", nom_com, "';"))
    
    subdiv <- st_read(conn, query = paste0(
      "SELECT sub.* 
         FROM subdiv sub
         JOIN parc_", num_departement, "_", temps_apres, " pa ON
           pa.idu = ANY (string_to_array(sub.participants, ','))
         WHERE pa.nom_com = '", nom_com, "';"))
    
    redecoupage <- st_read(conn, query = paste0(
      "SELECT red.* 
         FROM redecoupage red
         JOIN parc_", num_departement, "_", temps_apres, " pa ON
           pa.idu = ANY (string_to_array(red.participants_apres, ','))
         WHERE pa.nom_com = '", nom_com, "';"))
    
    contour_redecoupage <- st_read(conn, query = paste0(
      "SELECT cot.* 
         FROM contour_redecoupage cot
         JOIN parc_", num_departement, "_", temps_apres, " pa ON
           pa.idu = ANY (string_to_array(cot.participants_apres, ','))
         WHERE pa.nom_com = '", nom_com, "';"))
    
    contour_redecoupage_translation <- st_read(conn, query = paste0(
      "SELECT cott.* 
         FROM contour_redecoupage_translation cott
         JOIN parc_", num_departement, "_", temps_apres, " pa ON
           pa.idu = ANY (string_to_array(cott.participants_apres_translate, ','))
         WHERE pa.nom_com = '", nom_com, "';"))
    
    map_1 <- map_1 + mapview(defusion_com,  
                             layer.name = paste0("Parcelles défusion de communes (état 20",temps_apres,")"), 
                             col.regions = "white",
                             alpha.regions = 0.5, homebutton = F) +
      mapview(defusion_com_avant,  
              layer.name = paste0("Parcelles défusion de communes (état 20",temps_avant,")"), 
              col.regions = "white", alpha.regions = 0.5, homebutton = F)
    
  } else if (nrow(is_defusion_com) > 0) {
    
    defusion_com <- st_read(conn, query = paste0(
      "SELECT * FROM defusion_com WHERE nom_com = '", nom_com, "';"))
    
    defusion_com_avant <- st_read(conn, query = paste0(
      "SELECT * FROM parc_", num_departement, "_", temps_avant, 
      " WHERE idu IN (SELECT idu_avant FROM defusion_com WHERE nom_com = '", nom_com, "');"))
    
    parc_avant <- st_read(conn, query = paste0(
      "SELECT * FROM parc_", num_departement, "_", temps_avant, 
      " WHERE nom_com = '", nom_com, "'
           OR nom_com =
        (SELECT nom_com FROM chgt_commune 
          WHERE '", nom_com, "' = ANY(regexp_split_to_array(participants, ',\\s*')));"))
    
    modif_avant <- st_read(conn, query = paste0(
      "SELECT ma.* 
         FROM modif_avant ma
         JOIN parc_", num_departement, "_", temps_apres, " pa ON ma.idu = pa.idu
         WHERE ma.nom_com = 
              (SELECT nom_com FROM chgt_commune 
                WHERE '", nom_com, "' = ANY(regexp_split_to_array(participants, ',\\s*')))
           AND pa.nom_com = '", nom_com, "';"))
    
    supp <- st_read(conn, query =  paste0(
      "SELECT * FROM supp WHERE nom_com = '",nom_com, "'
           OR nom_com = 
              (SELECT nom_com FROM chgt_commune 
                WHERE '", nom_com, "' = ANY(regexp_split_to_array(participants, ',\\s*')));"))
    
    vrai_supp <- st_read(conn, query = paste0(
      "SELECT * FROM vrai_supp WHERE nom_com = '", nom_com, "'
           OR nom_com = 
              (SELECT nom_com FROM chgt_commune 
                WHERE '", nom_com, "' = ANY(regexp_split_to_array(participants, ',\\s*')));"))
    
    contour <- st_read(conn, query = paste0(
      "SELECT co.* 
         FROM contour co
         JOIN parc_", num_departement, "_", temps_apres, " pa ON
           pa.idu = ANY (string_to_array(co.participants_apres, ','))
         WHERE co.nom_com = 
              (SELECT nom_com FROM chgt_commune 
                WHERE '", nom_com, "' = ANY(regexp_split_to_array(participants, ',\\s*')))
           AND pa.nom_com = '", nom_com, "';"))
    
    contour_translation <- st_read(conn, query = paste0(
      "SELECT cot.* 
         FROM contour_translation cot
         JOIN parc_", num_departement, "_", temps_apres, " pa ON cot.idu_translate = pa.idu
         WHERE cot.nom_com = 
              (SELECT nom_com FROM chgt_commune 
                WHERE '", nom_com, "' = ANY(regexp_split_to_array(participants, ',\\s*')))
           AND pa.nom_com = '", nom_com, "';"))
    
    subdiv <- st_read(conn, query = paste0(
      "SELECT sub.* 
         FROM subdiv sub
         JOIN parc_", num_departement, "_", temps_apres, " pa ON
           pa.idu = ANY (string_to_array(sub.participants, ','))
         WHERE sub.nom_com = 
              (SELECT nom_com FROM chgt_commune 
                WHERE '", nom_com, "' = ANY(regexp_split_to_array(participants, ',\\s*')))
           AND pa.nom_com = '", nom_com, "';"))
    
    redecoupage <- st_read(conn, query = paste0(
      "SELECT red.* 
         FROM redecoupage red
         JOIN parc_", num_departement, "_", temps_apres, " pa ON
           pa.idu = ANY (string_to_array(red.participants_apres, ','))
         WHERE red.nom_com = 
              (SELECT nom_com FROM chgt_commune 
                WHERE '", nom_com, "' = ANY(regexp_split_to_array(participants, ',\\s*')))
           AND pa.nom_com = '", nom_com, "';"))
    
    contour_redecoupage <- st_read(conn, query = paste0(
      "SELECT cot.* 
         FROM contour_redecoupage cot
         JOIN parc_", num_departement, "_", temps_apres, " pa ON
           pa.idu = ANY (string_to_array(cot.participants_apres, ','))
         WHERE cot.nom_com = 
              (SELECT nom_com FROM chgt_commune 
                WHERE '", nom_com, "' = ANY(regexp_split_to_array(participants, ',\\s*')))
           AND pa.nom_com = '", nom_com, "';"))
    
    contour_redecoupage_translation <- st_read(conn, query = paste0(
      "SELECT cott.* 
         FROM contour_redecoupage_translation cott
         JOIN parc_", num_departement, "_", temps_apres, " pa ON
           pa.idu = ANY (string_to_array(cott.participants_apres_translate, ','))
         WHERE cott.nom_com = 
              (SELECT nom_com FROM chgt_commune 
                WHERE '", nom_com, "' = ANY(regexp_split_to_array(participants, ',\\s*')))
           AND pa.nom_com = '", nom_com, "';"))
    
    map_1 <- map_1 + mapview(defusion_com,  
                             layer.name = paste0("Parcelles défusion de communes (état 20",temps_apres,")"), 
                             col.regions = "white",
                             alpha.regions = 0.5, homebutton = F) +
      mapview(defusion_com_avant,  
              layer.name = paste0("Parcelles défusion de communes (état 20",temps_avant,")"), 
              col.regions = "white", alpha.regions = 0.5, homebutton = F)
    
  } else {
    modif_avant <- st_read(conn, query =  paste0(
      "SELECT * FROM modif_avant WHERE nom_com = '", nom_com, "';"))
    contour <- st_read(conn, query = paste0(
      "SELECT * FROM contour WHERE nom_com = '",nom_com, "';"))
    contour_translation <- st_read(conn, query = paste0(
      "SELECT * FROM contour_translation WHERE nom_com = '",nom_com, "';"))
    subdiv <- st_read(conn, query = paste0(
      "SELECT * FROM subdiv WHERE nom_com = '", nom_com, "';"))
    redecoupage <- st_read(conn, query = paste0(
      "SELECT * FROM redecoupage WHERE nom_com = '", nom_com, "';"))
    contour_redecoupage <- st_read(conn, query = paste0(
      "SELECT * FROM contour_redecoupage WHERE nom_com = '", nom_com, "';"))
    contour_redecoupage_translation <- st_read(conn, query = paste0(
      "SELECT * FROM contour_redecoupage_translation WHERE nom_com = '", nom_com, "';"))
  }
  
  if (nrow(translation) > 0) {
    
    map_1 <- map_1 + mapview(translation,
                             layer.name = paste0("Parcelles translatées (état 20",temps_apres,")"), 
                             col.regions = "#069F9C",
                             alpha.regions = 0.5, homebutton = F) +
      mapview(parc_avant %>%
                filter(idu %in% translation$idu_translate), 
              col.regions = "#069F9C",
              layer.name = paste0("Parcelles translatées (état 20",temps_avant,")"), 
              alpha.regions = 0.5, homebutton = F)

  }
  if (nrow(fusion) > 0) {
    
    map_1 <- map_1 + mapview(fusion, 
                             layer.name = paste0("Parcelles fusionnéees (état 20",temps_apres,")"),
                             col.regions = "#5F1972", alpha.regions = 0.5, homebutton = F) +
      mapview(parc_avant %>%
                filter(idu %in% unlist(str_split(fusion$participants, ",\\s*"))),  
              layer.name = paste0("Parcelles fusionnéees (état 20",temps_avant,")"), 
              col.regions = "#5F1972",
              alpha.regions = 0.5, homebutton = F)
  }
  if (nrow(subdiv) > 0) {
    
    map_1 <- map_1 + mapview(parc_apres %>%
                               filter(idu %in% unlist(str_split(subdiv$participants, ",\\s*"))),  
                             layer.name = paste0("Parcelles subdivisées (état 20",temps_apres,")"), 
                             col.regions = "#AE48C0",
                             alpha.regions = 0.5, homebutton = F) +
      mapview(subdiv,  
              layer.name = paste0("Parcelles subdivisées (état 20",temps_avant,")"), 
              col.regions = "#AE48C0", alpha.regions = 0.5, homebutton = F)
  }
  if (nrow(redecoupage) > 0) {
    
    map_1 <- map_1 + mapview(parc_apres %>%
                               filter(idu %in% unlist(str_split(redecoupage$participants_apres, ",\\s*"))),
                             layer.name = paste0("Parcelles redécoupage (état 20",temps_apres,")"),
                             col.regions = "#E0BDE6",
                             alpha.regions = 0.5, homebutton = F) +
      mapview(redecoupage,  
              layer.name = paste0("Parcelles redécoupage (état 20",temps_avant,")"), 
              col.regions = "#E0BDE6", alpha.regions = 0.5, homebutton = F)
  }
  if (nrow(contour) > 0) {
    
    map_1 <- map_1 + mapview(parc_apres %>%
                               filter(idu %in% unlist(str_split(contour$participants_apres, ",\\s*"))),  
                             layer.name = paste0("Parcelles contours (état 20",temps_apres,")"), 
                             col.regions = "#D79700", alpha.regions = 0.5, homebutton = F) +
      mapview(contour,  
              layer.name = paste0("Parcelles contours (état 20",temps_avant,")"),
              col.regions = "#D79700",
              alpha.regions = 0.5, homebutton = F)
  }
  if (nrow(contour_redecoupage) > 0) {
    
    map_1 <- map_1 + mapview(parc_apres %>%
                               filter(idu %in% unlist(str_split(contour_redecoupage$participants_apres, ",\\s*"))),  
                             layer.name = paste0("Parcelles redecoupage + contours (état 20",temps_apres,")"),
                             col.regions = "#FFB9BB",
                             alpha.regions = 0.5, homebutton = F) +
      mapview(contour_redecoupage,
              layer.name = paste0("Parcelles redecoupage + contours (état 20",temps_avant,")"), 
              col.regions = "#FFB9BB", alpha.regions = 0.5, homebutton = F)
  }
  if (nrow(contour_translation) > 0) {
    
    map_1 <- map_1 + mapview(contour_translation,  
                             layer.name = paste0("Parcelles translatées + contours (état 20",temps_apres,")"), 
                             alpha.regions = 0.5, homebutton = F) +
      mapview(parc_avant %>%
                filter(idu %in% contour_translation$idu_translate),
              layer.name = paste0("Parcelles translatées + contours (état 20",temps_avant,")"), 
              alpha.regions = 0.5, homebutton = F)
    
  }
  if (nrow(contour_redecoupage_translation) > 0) {
    
    map_1 <- map_1 + mapview(parc_apres %>%
                               filter(idu %in% unlist(str_split(contour_redecoupage_translation$participants_apres_translate, ",\\s*"))),
                             layer.name = paste0("Parcelles redecoupage + translatées + contours (état 20",temps_apres,")"), 
                             col.regions = "#B5E1FF", alpha.regions = 0.5, homebutton = F) +
      mapview(contour_redecoupage_translation,  
              layer.name = paste0("Parcelles redecoupage + translatées + contours (état 20",temps_avant,")"), 
              col.regions = "#B5E1FF",
              alpha.regions = 0.5, homebutton = F)
  }
  if (nrow(vrai_ajout[!st_is_empty(vrai_ajout), ]) > 0) {
    
    map_1 <- map_1 + mapview(vrai_ajout, 
                             layer.name = "Parcelles ajoutées", 
                             col.regions = "#26A44B", alpha.regions = 0.5, homebutton = F)
  }
  if (nrow(vrai_supp[!st_is_empty(vrai_supp), ]) > 0) {
    
    map_1 <- map_1 + mapview(vrai_supp,
                             layer.name = "Parcelles supprimées",
                             col.regions = "#E91422", alpha.regions = 0.5, homebutton = F) 
  }
  if (nrow(ajout) > 0) {
    
    map_1 <- map_1 + mapview(ajout, col.regions = "#DAF7E2",
                             layer.name = paste0("Parcelles restantes (état 20",temps_apres,")"), 
                             alpha.regions = 0.5, homebutton = F)
  }
  if (nrow(supp) > 0) {
    
    map_1 <- map_1 + mapview(supp, col.regions = "#FFE2E2",
                             layer.name = paste0("Parcelles restantes (état 20",temps_avant,")"), 
                             alpha.regions = 0.5, homebutton = F)
  }
  if (nrow(modif_apres) > 0) {
    
    map_1 <- map_1 + mapview(modif_apres,  col.regions = "#DAF7E2",
                             layer.name = paste0("Parcelles modifiées restantes (état 20",temps_apres,")"),
                             alpha.regions = 0.5, homebutton = F)
  }
  if (nrow(modif_avant) > 0) {
    
    map_1 <- map_1 + mapview(modif_avant, col.regions = "#FFE2E2",
                             layer.name = paste0("Parcelles modifiées restantes (état 20",temps_avant,")"),
                             alpha.regions = 0.5, homebutton = F)
  }
  
  map_2 <- mapview(parc_apres, 
                   layer.name = paste0("Parcelles (état 20",temps_apres,")"),
                   col.regions = "#286AC7", 
                   homebutton = FALSE) + map_base
  
  map_3 <- mapview(parc_avant, 
                   layer.name = paste0("Parcelles (état 20",temps_avant,")"), 
                   col.regions = "#FFC300",
                   homebutton = FALSE) + map_base
  
  map_compa <- map_2 | map_3
  
  sync(map_1, map_compa@map, ncol = 1)
}

check_refonte_pc <- function(conn, nom_com, seuil=50) {
  nb_parcelles_rest <- dbGetQuery(conn, paste0("
  SELECT 
    (SELECT COUNT(*) 
     FROM modif_avant
     WHERE nom_com = '", nom_com, "') AS nb_modif_restant_avant,
    (SELECT COUNT(*) 
     FROM supp
     WHERE nom_com = '", nom_com, "') AS nb_supp_restant,
    (SELECT COUNT(*) 
     FROM modif_avant
     WHERE nom_com = '", nom_com, "') +
    (SELECT COUNT(*) 
     FROM supp
     WHERE nom_com = '", nom_com, "') AS total_count
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
      modif_restant_avant AS (
        SELECT nom_com, COUNT(*) AS nb_modif_restant_avant
        FROM modif_avant
        GROUP BY nom_com
      ),
      supp_restant AS (
        SELECT nom_com, COUNT(*) AS nb_supp_restant
        FROM supp
        GROUP BY nom_com
      ),
      modif_restant_apres AS (
        SELECT nom_com, COUNT(*) AS nb_modif_restant_apres
        FROM modif_apres
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
      contour_translation AS (
        SELECT nom_com, COUNT(*) AS nb_contour_translation
        FROM contour_translation
        GROUP BY nom_com
      ),
      subdiv AS (
        SELECT nom_com, COUNT(*) AS nb_subdiv
        FROM subdiv
        GROUP BY nom_com
      ),
      fusion AS (
        SELECT nom_com, COUNT(*) AS nb_fusion
        FROM fusion
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
      contour_redecoupage_translation AS (
        SELECT nom_com, COUNT(DISTINCT participants_avant_translate) AS nb_contour_redecoupage_translation
        FROM contour_redecoupage_translation
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
        base.nom_com,
        code_com,
        parcelles_20", temps_apres,",
        parcelles_20", temps_avant,",
        COALESCE(nb_modif_restant_apres, 0) + COALESCE(nb_ajout_restant, 0) AS restantes_20", temps_apres,",
        COALESCE(nb_modif_restant_avant, 0) + COALESCE(nb_supp_restant, 0) AS restantes_20", temps_avant,",
        COALESCE(nb_ajout, 0) AS ajout,
        COALESCE(nb_supp, 0) AS suppression,
        COALESCE(nb_translation, 0) AS translation,
        COALESCE(nb_contour, 0) AS contour,
        COALESCE(nb_contour_translation, 0) AS contour_translation,
        COALESCE(nb_subdiv, 0) AS subdivision,
        COALESCE(nb_fusion, 0) AS fusion,
        COALESCE(nb_redecoupage, 0) AS redecoupage,
        COALESCE(nb_contour_redecoupage, 0) AS contour_redecoupage,
        COALESCE(nb_contour_redecoupage_translation, 0) AS contour_redecoupage_translation
      FROM base
      FULL OUTER JOIN modif_restant_avant ON base.nom_com = modif_restant_avant.nom_com
      FULL OUTER JOIN supp_restant ON base.nom_com = supp_restant.nom_com
      FULL OUTER JOIN modif_restant_apres ON base.nom_com = modif_restant_apres.nom_com
      FULL OUTER JOIN ajout_restant ON base.nom_com = ajout_restant.nom_com
      FULL OUTER JOIN vrai_ajout ON base.nom_com = vrai_ajout.nom_com
      FULL OUTER JOIN vrai_supp ON base.nom_com = vrai_supp.nom_com
      FULL OUTER JOIN translation ON base.nom_com = translation.nom_com
      FULL OUTER JOIN contour ON base.nom_com = contour.nom_com
      FULL OUTER JOIN contour_translation ON base.nom_com = contour_translation.nom_com
      FULL OUTER JOIN subdiv ON base.nom_com = subdiv.nom_com
      FULL OUTER JOIN fusion ON base.nom_com = fusion.nom_com
      FULL OUTER JOIN redecoupage ON base.nom_com = redecoupage.nom_com
      FULL OUTER JOIN contour_redecoupage ON base.nom_com = contour_redecoupage.nom_com
      FULL OUTER JOIN contour_redecoupage_translation ON base.nom_com = contour_redecoupage_translation.nom_com;"))
  
  final_df <- final_df %>%
    mutate(across(where(bit64::is.integer64), as.integer))
  
  datatable(final_df[, var], 
            options = list(pageLength = 15, 
                           autoWidth = TRUE, 
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