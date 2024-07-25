library(shiny)
library(sf)
library(mapview)
library(leaflet)
library(stringr)
library(leafsync)
library(leaflet.extras2)
library(DBI)
library(DT)
rm(list=ls())
source(file = "database/connexion_db.R")
conn <- connecter()

result <- dbGetQuery(conn, paste0(" 
  SELECT 
      schema_name
  FROM 
      information_schema.schemata
  WHERE 
      schema_name LIKE 'traitement_%_%_cadastre_%';
           "))

departements <- result$schema_name %>%
  str_extract("cadastre_(..)\\z") %>%
  str_replace_all("cadastre_", "") %>%
  unique() %>%
  sort()

# Fonction pour mettre à jour le search path
update_search_path <- function(conn, departement, temps_apres, temps_avant) {
  dbExecute(conn, paste0(
    "SET search_path TO traitement_", temps_apres, "_", temps_avant, "_cadastre_" , departement, 
    ", cadastre_", departement, ", public"
  ))
  print(paste("temps_apres:", temps_apres, ", temps_avant:", temps_avant, 
              ", departement:", departement))
}

# Fonction pour lire les communes
load_communes <- function(conn, departement) {
  dbGetQuery(conn, paste0("SELECT nom_com FROM cadastre_", departement, ".com_", departement, ";"))
}

# Define UI
ui <- fluidPage(
  titlePanel(
    div(
      img(src = "Insee_logo.png", height = "60px", align = "right"),
      h1("Détection des évolutions des parcelles cadastrales")
    )
  ),
  
  # Navigation panel
  tabsetPanel(
    id = "tabsetPanel",  # Give an ID to the tabsetPanel
    type = "tabs",
    tabPanel("Carte des comparaison par commune",
             br(),
             fluidRow(
               column(3,
                      selectInput("depart_select", "Choisir un département:",
                                  choices = departements,
                                  selected = departements[length(departements)])
               ),
               column(3,
                      selectInput("temps_select", "Choisir une période de temps:",
                                  choices = NULL,
                                  selected = NULL)
               ),
               column(3,
                      selectInput("nom_com_select", "Choisir un nom de commune:",
                                  choices = NULL,
                                  selected = NULL)
               ),
             ),
             uiOutput("dynamicMaps")  # Use UI output to render maps
    ),
    tabPanel("Liste des changments par département",
             br(),
             fluidRow(
               column(3,
                      selectInput("depart_select", "Choisir un département:",
                                  choices = departements,
                                  selected = departements[length(departements)])
               ),
               column(3,
                      selectInput("temps_select", "Choisir une période de temps:",
                                  choices = NULL,
                                  selected = NULL)
               ),
             ),
             DTOutput("table")
    ),
  ),
  
  tags$style(HTML("
    .leaflet {
      height: 100%;
    }
    .shiny-output-container {
      margin: 0;  /* Remove margins */
      padding: 0; /* Remove padding */
    }
    .selectize-dropdown {
    z-index: 2000; /* Valeur plus élevée pour être au-dessus des cartes */
  }
  "))
)

# Define server logic
server <- function(input, output, session) {
  
  # Mise à jour lors de la sélection d'un département
  observeEvent(input$depart_select, {
    num_departement <<- input$depart_select
    print(paste0("Changement au niveau du département: ", num_departement))
    commune <- load_communes(conn, num_departement)
    
    updateSelectInput(session, "nom_com_select",
                      choices = sort(unique(commune$nom_com)),
                      selected = sort(unique(commune$nom_com))[1])
    
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
    
    updateSelectInput(session, "temps_select",
                      choices = int_temps,
                      selected = int_temps[1])
    
  })
  
  temps_reactive <- reactive({
    req(input$temps_select)  # Assure que input$temps_select n'est pas NULL
    req(input$depart_select)
    print(paste0("Changement au niveau de la période de temps: ", input$temps_select))
    temps_split <- strsplit(input$temps_select, "-")[[1]]
    temps_apres <<- temps_split[1]
    temps_avant <<- temps_split[2]
    update_search_path(conn, num_departement, temps_apres, temps_avant)
  })
  
  # Rendu dynamique des cartes
  output$dynamicMaps <- renderUI({
    req(input$tabsetPanel == "Carte des comparaison par commune")
    req(input$nom_com_select)
    req(input$temps_select)
    # Assurer que temps_reactive est terminé avant de continuer
    isolate({
      temps_reactive()  # Ce qui déclenche l'exécution de temps_reactive
    })
    print(paste0("Affichage des cartes pour la commune: ", input$nom_com_select))
    nom_com_select <- gsub("'", "''", input$nom_com_select)
    
    bordure <- st_read(conn, query = paste0(
      "SELECT * FROM bordure WHERE nom_com = '", nom_com_select, "';"))
    ins_parc_avant <- st_read(conn, query = paste0(
      "SELECT * FROM parc_", num_departement, "_", temps_avant, " WHERE nom_com = '", nom_com_select, "';"))
    ins_parc_apres <- st_read(conn, query = paste0(
      "SELECT * FROM parc_", num_departement, "_", temps_apres, " WHERE nom_com = '", nom_com_select, "';"))
    
    modif_avant <- st_read(conn, query =  paste0(
      "SELECT * FROM modif_avant_iou_convex WHERE nom_com = '",nom_com_select, "';"))
    modif_apres <- st_read(conn, query =  paste0(
      "SELECT * FROM modif_apres_iou WHERE nom_com = '",nom_com_select, "';"))
    ajout <- st_read(conn, query =  paste0(
      "SELECT * FROM ajout_iou_restant WHERE nom_com = '",nom_com_select, "';"))
    supp <- st_read(conn, query =  paste0(
      "SELECT * FROM supp_iou_restant WHERE nom_com = '",nom_com_select, "';"))
    
    translation <- st_read(conn, query = paste0(
      "SELECT * FROM translation WHERE nom_com = '",nom_com_select, "';"))
    contour <- st_read(conn, query = paste0(
      "SELECT * FROM contour WHERE nom_com = '",nom_com_select, "';"))
    contour_translation <- st_read(conn, query = paste0(
      "SELECT * FROM contour_translation WHERE nom_com = '",nom_com_select, "';"))
    defusion_com <- st_read(conn, query = paste0(
      "SELECT * FROM defusion_com WHERE nom_com = '", nom_com_select, "';"))
    fusion_com <- st_read(conn, query = paste0(
      "SELECT * FROM fusion_com WHERE nom_com = '", nom_com_select, "';"))
    subdiv <- st_read(conn, query = paste0(
      "SELECT * FROM subdiv WHERE nom_com = '", nom_com_select, "';"))
    fusion <- st_read(conn, query = paste0(
      "SELECT * FROM fusion WHERE nom_com = '", nom_com_select, "';"))
    redecoupage <- st_read(conn, query = paste0(
      "SELECT * FROM redecoupage WHERE nom_com = '", nom_com_select, "';"))
    contour_transfo <- st_read(conn, query = paste0(
      "SELECT * FROM contour_transfo WHERE nom_com = '", nom_com_select, "';"))
    contour_transfo_translation <- st_read(conn, query = paste0(
      "SELECT * FROM contour_transfo_translation WHERE nom_com = '", nom_com_select, "';"))
    vrai_ajout <- st_read(conn, query = paste0(
      "SELECT * FROM vrai_ajout WHERE nom_com = '", nom_com_select, "';"))
    vrai_supp <- st_read(conn, query = paste0(
      "SELECT * FROM vrai_supp WHERE nom_com = '", nom_com_select, "';"))
    
    map_1 <- mapview(bordure, 
                     layer.name = "Bordures étendues", col.regions = "#E8E8E8", 
                     alpha.regions = 0.7, homebutton = F, 
                     map.types = c("CartoDB.Positron", "OpenStreetMap", "Esri.WorldImagery"))
    
    if (nrow(defusion_com) > 0) {
      
      
      defusion_com_avant <- st_read(conn, query = paste0(
        "SELECT * FROM parc_", num_departement, "_", temps_avant, 
        " WHERE idu IN (SELECT idu_avant FROM defusion_com WHERE nom_com = '", nom_com_select, "');"))
      
      map_1 <- map_1 + mapview(defusion_com,  
                               layer.name = paste0("Parcelles défusion de communes (état 20",temps_apres,")"), 
                               col.regions = "white",
                               alpha.regions = 0.5, homebutton = F) +
        mapview(defusion_com_avant,  
                layer.name = paste0("Parcelles défusion de communes (état 20",temps_avant,")"), 
                col.regions = "white", alpha.regions = 0.5, homebutton = F)
      
      nom_com_defusion <- dbGetQuery(conn, paste0("
        SELECT DISTINCT nom_com FROM parc_", num_departement, "_", temps_avant, " 
        WHERE idu IN 
            (SELECT idu_avant FROM defusion_com 
              WHERE nom_com = '", nom_com_select, "');"))
      
      nom_com_defusion <- paste0("'", paste(nom_com_defusion$nom_com, collapse = "', '"), "'")
      
      ins_parc_avant <- st_read(conn, query = paste0(
        "SELECT * FROM parc_", num_departement, "_", temps_avant, 
        " WHERE nom_com = '", nom_com_select, "'
           OR nom_com IN (",nom_com_defusion,");"))
      
      modif_avant <- st_read(conn, query = paste0(
        "SELECT mav.* 
         FROM modif_avant_iou_convex mav
         JOIN parc_", num_departement, "_", temps_apres, " pa ON mav.idu = pa.idu
         WHERE mav.nom_com IN (", nom_com_defusion, ")
           AND pa.nom_com = '", nom_com_select, "';"))
      
      supp <- st_read(conn, query =  paste0(
        "SELECT * FROM supp_iou_restant WHERE nom_com = '",nom_com_select, "'
           OR nom_com IN (",nom_com_defusion,");"))
      
      vrai_supp <- st_read(conn, query = paste0(
        "SELECT * FROM vrai_supp WHERE nom_com = '", nom_com_select, "'
           OR nom_com IN (",nom_com_defusion,");"))
      
      contour <- st_read(conn, query = paste0(
        "SELECT co.* 
         FROM contour co
         JOIN parc_", num_departement, "_", temps_apres, " pa ON
           pa.idu = ANY (string_to_array(co.participants_apres, ','))
         WHERE co.nom_com IN (", nom_com_defusion, ")
           AND pa.nom_com = '", nom_com_select, "';"))
      
      contour_translation <- st_read(conn, query = paste0(
        "SELECT cot.* 
         FROM contour_translation cot
         JOIN parc_", num_departement, "_", temps_apres, " pa ON cot.idu_translate = pa.idu
         WHERE cot.nom_com IN (", nom_com_defusion, ")
           AND cot.nom_com = '", nom_com_select, "';"))
      
      subdiv <- st_read(conn, query = paste0(
        "SELECT sub.* 
         FROM subdiv sub
         JOIN parc_", num_departement, "_", temps_apres, " pa ON
           pa.idu = ANY (string_to_array(sub.participants, ','))
         WHERE sub.nom_com IN (", nom_com_defusion, ")
           AND pa.nom_com = '", nom_com_select, "';"))
      
      redecoupage <- st_read(conn, query = paste0(
        "SELECT red.* 
         FROM redecoupage red
         JOIN parc_", num_departement, "_", temps_apres, " pa ON
           pa.idu = ANY (string_to_array(red.participants_apres, ','))
         WHERE red.nom_com IN (", nom_com_defusion, ")
           AND pa.nom_com = '", nom_com_select, "';"))
      
      contour_transfo <- st_read(conn, query = paste0(
        "SELECT cot.* 
         FROM contour_transfo cot
         JOIN parc_", num_departement, "_", temps_apres, " pa ON
           pa.idu = ANY (string_to_array(cot.participants_apres, ','))
         WHERE cot.nom_com IN (", nom_com_defusion, ")
           AND pa.nom_com = '", nom_com_select, "';"))
      
      contour_transfo_translation <- st_read(conn, query = paste0(
        "SELECT cott.* 
         FROM contour_transfo_translation cott
         JOIN parc_", num_departement, "_", temps_apres, " pa ON
           pa.idu = ANY (string_to_array(cott.participants_apres_translate, ','))
         WHERE cott.nom_com IN (", nom_com_defusion, ")
           AND pa.nom_com = '", nom_com_select, "';"))
    }
    if (nrow(fusion_com) > 0) {
      
      fusion_com_avant <- st_read(conn, query = paste0(
        "SELECT * FROM parc_", num_departement, "_", temps_avant, 
        " WHERE idu IN 
            (SELECT idu_avant FROM fusion_com WHERE nom_com = '", nom_com_select, "');"))
      
      map_1 <- map_1 + mapview(fusion_com,  
                               layer.name = paste0("Parcelles fusion de communes (état 20",temps_apres,")"), 
                               col.regions = "white",
                               alpha.regions = 0.5, homebutton = F) +
        mapview(fusion_com_avant,  
                layer.name = paste0("Parcelles fusion de communes (état 20",temps_avant,")"), 
                col.regions = "white", alpha.regions = 0.5, homebutton = F)
      
      nom_com_fusion <- dbGetQuery(conn, paste0("
        SELECT DISTINCT nom_com FROM parc_", num_departement, "_", temps_avant, " 
        WHERE idu IN 
            (SELECT idu_avant FROM fusion_com 
              WHERE nom_com = '", nom_com_select, "');"))
      
      nom_com_fusion <- paste0("'", paste(nom_com_fusion$nom_com, collapse = "', '"), "'")
      
      ins_parc_avant <- st_read(conn, query = paste0(
        "SELECT * FROM parc_", num_departement, "_", temps_avant, 
        " WHERE nom_com = '", nom_com_select, "'
           OR nom_com IN (",nom_com_fusion,");"))
      modif_avant <- st_read(conn, query =  paste0(
        "SELECT * FROM modif_avant_iou_convex WHERE nom_com = '",nom_com_select, "'
           OR nom_com IN (",nom_com_fusion,");"))
      supp <- st_read(conn, query =  paste0(
        "SELECT * FROM supp_iou_restant WHERE nom_com = '",nom_com_select, "'
           OR nom_com IN (",nom_com_fusion,");"))
      contour <- st_read(conn, query = paste0(
        "SELECT * FROM contour WHERE nom_com = '",nom_com_select, "'
           OR nom_com IN (",nom_com_fusion,");"))
      contour_translation <- st_read(conn, query = paste0(
        "SELECT * FROM contour_translation WHERE nom_com = '",nom_com_select, "'
           OR nom_com IN (",nom_com_fusion,");"))
      subdiv <- st_read(conn, query = paste0(
        "SELECT * FROM subdiv WHERE nom_com = '", nom_com_select, "'
           OR nom_com IN (",nom_com_fusion,");"))
      redecoupage <- st_read(conn, query = paste0(
        "SELECT * FROM redecoupage WHERE nom_com = '", nom_com_select, "'
           OR nom_com IN (",nom_com_fusion,");"))
      contour_transfo <- st_read(conn, query = paste0(
        "SELECT * FROM contour_transfo WHERE nom_com = '", nom_com_select, "'
           OR nom_com IN (",nom_com_fusion,");"))
      contour_transfo_translation <- st_read(conn, query = paste0(
        "SELECT * FROM contour_transfo_translation WHERE nom_com = '", nom_com_select, "'
           OR nom_com IN (",nom_com_fusion,");"))
      vrai_supp <- st_read(conn, query = paste0(
        "SELECT * FROM vrai_supp WHERE nom_com = '", nom_com_select, "'
           OR nom_com IN (",nom_com_fusion,");"))
    }
    if (nrow(translation) > 0) {
      
      map_1 <- map_1 + mapview(translation,
                               layer.name = paste0("Parcelles translatées (état 20",temps_apres,")"), 
                               col.regions = "darkcyan",
                               alpha.regions = 0.5, homebutton = F) +
        mapview(ins_parc_avant %>%
                  filter(idu %in% translation$idu_translate), 
                col.regions = "darkcyan",
                layer.name = "Parcelles translatées (état 2023)", 
                alpha.regions = 0.5, homebutton = F)
    }
    if (nrow(fusion) > 0) {
      
      map_1 <- map_1 + mapview(fusion, 
                               layer.name = paste0("Parcelles fusionnéees (état 20",temps_apres,")"),
                               col.regions = "darkmagenta", alpha.regions = 0.5, homebutton = F) +
        mapview(ins_parc_avant %>%
                  filter(idu %in% unlist(str_split(fusion$participants, ",\\s*"))),  
                layer.name = paste0("Parcelles fusionnéees (état 20",temps_avant,")"), 
                col.regions = "darkmagenta",
                alpha.regions = 0.5, homebutton = F)
    }
    if (nrow(subdiv) > 0) {
      
      map_1 <- map_1 + mapview(ins_parc_apres %>%
                                 filter(idu %in% unlist(str_split(subdiv$participants, ",\\s*"))),  
                               layer.name = paste0("Parcelles subdivisées (état 20",temps_apres,")"), 
                               col.regions = "purple",
                               alpha.regions = 0.5, homebutton = F) +
        mapview(subdiv,  
                layer.name = paste0("Parcelles subdivisées (état 20",temps_avant,")"), 
                col.regions = "purple", alpha.regions = 0.5, homebutton = F)
    }
    if (nrow(redecoupage) > 0) {
      
      map_1 <- map_1 + mapview(ins_parc_apres %>%
                                 filter(idu %in% unlist(str_split(redecoupage$participants_apres, ",\\s*"))),
                               layer.name = paste0("Parcelles redécoupage (état 20",temps_apres,")"),
                               col.regions = "magenta",
                               alpha.regions = 0.5, homebutton = F) +
        mapview(redecoupage,  
                layer.name = paste0("Parcelles redécoupage (état 20",temps_avant,")"), 
                col.regions = "magenta", alpha.regions = 0.5, homebutton = F)
    }
    if (nrow(contour) > 0) {
      
      map_1 <- map_1 + mapview(ins_parc_apres %>%
                                 filter(idu %in% unlist(str_split(contour$participants_apres, ",\\s*"))),  
                               layer.name = paste0("Parcelles contours (état 20",temps_apres,")"), 
                               col.regions = "orange", alpha.regions = 0.5, homebutton = F) +
        mapview(contour,  
                layer.name = paste0("Parcelles contours (état 20",temps_avant,")"),
                col.regions = "orange",
                alpha.regions = 0.5, homebutton = F)
    }
    if (nrow(contour_transfo) > 0) {
      
      map_1 <- map_1 + mapview(ins_parc_apres %>%
                                 filter(idu %in% unlist(str_split(contour_transfo$participants_apres, ",\\s*"))),  
                               layer.name = paste0("Parcelles transfo + contours (état 20",temps_apres,")"),
                               col.regions = "pink",
                               alpha.regions = 0.5, homebutton = F) +
        mapview(contour_transfo,
                layer.name = paste0("Parcelles transfo + contours (état 20",temps_avant,")"), 
                col.regions = "pink", alpha.regions = 0.5, homebutton = F)
    }
    if (nrow(contour_translation) > 0) {
      
      map_1 <- map_1 + mapview(contour_translation,  
                               layer.name = paste0("Parcelles translatées + contours (état 20",temps_apres,")"), 
                               alpha.regions = 0.5, homebutton = F) +
        mapview(ins_parc_avant %>%
                  filter(idu %in% contour_translation$idu_translate),
                layer.name = paste0("Parcelles translatées + contours (état 20",temps_avant,")"), 
                alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(contour_transfo_translation) > 0) {
      
      map_1 <- map_1 + mapview(ins_parc_apres %>%
                                 filter(idu %in% unlist(str_split(contour_transfo_translation$participants_apres_translate, ",\\s*"))),
                               layer.name = paste0("Parcelles transfo + translatées + contours (état 20",temps_apres,")"), 
                               col.regions = "lightblue", alpha.regions = 0.5, homebutton = F) +
        mapview(contour_transfo_translation,  
                layer.name = paste0("Parcelles transfo + translatées + contours (état 20",temps_avant,")"), 
                col.regions = "lightblue",
                alpha.regions = 0.5, homebutton = F)
    }
    if (nrow(vrai_ajout) > 0) {
      
      map_1 <- map_1 + mapview(vrai_ajout, 
                               layer.name = "Parcelles véritablement ajoutées", 
                               col.regions = "green", alpha.regions = 0.5, homebutton = F)
    }
    if (nrow(vrai_supp) > 0) {
      
      map_1 <- map_1 + mapview(vrai_supp,
                               layer.name = "Parcelles véritablement supprimées",
                               col.regions = "red", alpha.regions = 0.5, homebutton = F) 
    }
    if (nrow(ajout) > 0) {
      
      map_1 <- map_1 + mapview(ajout,
                               z = c("iou_ajust"), layer.name = paste0("Parcelles restantes (état 20",temps_apres,")"), 
                               alpha.regions = 0.5, homebutton = F)
    }
    if (nrow(supp) > 0) {
      
      map_1 <- map_1 + mapview(supp, 
                               z = c("iou_multi"), layer.name = paste0("Parcelles restantes (état 20",temps_avant,")"), 
                               alpha.regions = 0.5, homebutton = F)
    }
    if (nrow(modif_apres) > 0) {
      
      map_1 <- map_1 + mapview(modif_apres, 
                               z = c("iou_ajust"), 
                               layer.name = paste0("Parcelles modifiées restantes (état 20",temps_apres,")"),
                               alpha.regions = 0.5, homebutton = F)
    }
    if (nrow(modif_avant) > 0) {
      
      map_1 <- map_1 + mapview(modif_avant, 
                               z = c("iou_ajust"), 
                               layer.name = paste0("Parcelles modifiées restantes (état 20",temps_avant,")"),
                               alpha.regions = 0.5, homebutton = F)
    }
    
    map_2 <- mapview(bordure, 
                     layer.name = "Bordures étendues", 
                     col.regions = "#E8E8E8", 
                     alpha.regions = 0.7, 
                     homebutton = FALSE, map.types = "CartoDB.Positron") + 
      mapview(ins_parc_apres, 
              layer.name = paste0("Parcelles (état 20",temps_apres,")"),
              col.regions = "#286AC7", 
              homebutton = FALSE)
    
    map_3 <- mapview(bordure, 
                     layer.name = "Bordures étendues", 
                     col.regions = "#E8E8E8", 
                     alpha.regions = 0.7, legend = F,
                     homebutton = FALSE, map.types = "CartoDB.Positron") + 
      mapview(ins_parc_avant, 
              layer.name = paste0("Parcelles (état 20",temps_avant,")"), 
              col.regions = "#FFC300",
              homebutton = FALSE) 
    
    map_compa <- map_2 | map_3
    
    sync(map_1, map_compa@map, ncol = 1)
    
  })
  
  output$table <- renderDT({
    req(input$tabsetPanel == "Liste des changments par département")
    isolate({
      temps_reactive()  # Ce qui déclenche l'exécution de temps_reactive
    })
    print(paste0("Affichage de la liste des changements au niveau du département: ", input$depart_select))
    
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
      modif_restant_avant AS(
        SELECT nom_com, COUNT(*) AS nb_modif_restant_avant
        FROM modif_avant_iou_convex 
        GROUP BY nom_com
      ),
      supp_restant AS(
        SELECT nom_com, COUNT(*) AS nb_supp_restant
        FROM supp_iou_restant
        GROUP BY nom_com
      ),
      modif_restant_apres AS(
        SELECT nom_com, COUNT(*) AS nb_modif_restant_apres
        FROM modif_apres_iou 
        GROUP BY nom_com
      ),
      ajout_restant AS(
        SELECT nom_com, COUNT(*) AS nb_ajout_restant
        FROM ajout_iou_restant
        GROUP BY nom_com
      ),
      vrai_ajout AS(
        SELECT nom_com, COUNT(*) AS nb_ajout
        FROM vrai_ajout
        GROUP BY nom_com
      ),
      vrai_supp AS(
        SELECT nom_com, COUNT(*) AS nb_supp
        FROM vrai_supp
        GROUP BY nom_com
      ),
      translation AS(
        SELECT nom_com, COUNT(*) AS nb_translation
        FROM translation
        GROUP BY nom_com
      ),
      contour AS(
        SELECT nom_com, COUNT(*) AS nb_contour
        FROM contour
        GROUP BY nom_com
      ),
      contour_translation AS(
        SELECT nom_com, COUNT(*) AS nb_contour_translation
        FROM contour_translation
        GROUP BY nom_com
      ),
      subdiv AS(
        SELECT nom_com, COUNT(*) AS nb_subdiv
        FROM subdiv
        GROUP BY nom_com
      ),
      fusion AS(
        SELECT nom_com, COUNT(*) AS nb_fusion
        FROM fusion
        GROUP BY nom_com
      ),
      redecoupage AS(
        SELECT nom_com, COUNT(DISTINCT participants_avant) AS nb_redecoupage
        FROM redecoupage
        GROUP BY nom_com
      ),
      contour_transfo AS(
        SELECT nom_com, COUNT(DISTINCT participants_avant) AS nb_contour_transfo
        FROM contour_transfo
        GROUP BY nom_com
      ),
      contour_transfo_translation AS(
        SELECT nom_com, COUNT(DISTINCT participants_avant_translate) AS nb_contour_transfo_translation
        FROM contour_transfo_translation
        GROUP BY nom_com
      )
      SELECT 
        COALESCE(parc_avant.nom_com, parc_apres.nom_com) AS nom_com,
        COALESCE(parc_avant.code_com, parc_apres.code_com) AS code_com,
        COALESCE(nb_parcelles_temps_apres, 0) AS parcelles_20",temps_apres,",
        COALESCE(nb_parcelles_temps_avant, 0) AS parcelles_20",temps_avant,",
        COALESCE(nb_modif_restant_apres, 0) + COALESCE(nb_ajout_restant, 0) AS parcelles_restantes_20",temps_apres,",
        COALESCE(nb_modif_restant_avant, 0) + COALESCE(nb_supp_restant, 0) AS parcelles_restantes_20",temps_avant,",
        COALESCE(nb_ajout, 0) AS vrai_ajout,
        COALESCE(nb_supp, 0) AS vrai_supp,
        COALESCE(nb_translation, 0) AS translation,
        COALESCE(nb_contour, 0) AS contour,
        COALESCE(nb_contour_translation, 0) AS contour_translation,
        COALESCE(nb_subdiv, 0) AS subdiv,
        COALESCE(nb_fusion, 0) AS fusion,
        COALESCE(nb_redecoupage, 0) AS redecoupage,
        COALESCE(nb_contour_transfo, 0) AS contour_transfo,
        COALESCE(nb_contour_transfo_translation, 0) AS contour_transfo_translation
      FROM parc_avant
      FULL OUTER JOIN parc_apres ON parc_avant.nom_com = parc_apres.nom_com
      FULL OUTER JOIN modif_restant_avant ON parc_avant.nom_com = modif_restant_avant.nom_com OR parc_apres.nom_com= modif_restant_avant.nom_com
      FULL OUTER JOIN supp_restant ON parc_avant.nom_com = supp_restant.nom_com
      FULL OUTER JOIN modif_restant_apres ON parc_avant.nom_com = modif_restant_apres.nom_com
      FULL OUTER JOIN ajout_restant ON parc_avant.nom_com = ajout_restant.nom_com
      FULL OUTER JOIN vrai_ajout ON parc_avant.nom_com = vrai_ajout.nom_com
      FULL OUTER JOIN vrai_supp ON parc_avant.nom_com = vrai_supp.nom_com
      FULL OUTER JOIN translation ON parc_avant.nom_com = translation.nom_com
      FULL OUTER JOIN contour ON parc_avant.nom_com = contour.nom_com
      FULL OUTER JOIN contour_translation ON parc_avant.nom_com = contour_translation.nom_com
      FULL OUTER JOIN subdiv ON parc_avant.nom_com = subdiv.nom_com
      FULL OUTER JOIN fusion ON parc_avant.nom_com = fusion.nom_com
      FULL OUTER JOIN redecoupage ON parc_avant.nom_com = redecoupage.nom_com
      FULL OUTER JOIN contour_transfo ON parc_avant.nom_com = contour_transfo.nom_com
      FULL OUTER JOIN contour_transfo_translation ON parc_avant.nom_com = contour_transfo_translation.nom_com;"))
    
    final_df <- final_df %>%
      mutate(across(where(bit64::is.integer64), as.integer))
    
    datatable(final_df, options = list(pageLength = 15, autoWidth = TRUE, ordering = TRUE), rownames = FALSE)
  })
}

# defusion fusion
shinyApp(ui, server)
