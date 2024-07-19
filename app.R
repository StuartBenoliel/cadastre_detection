library(shiny)
library(sf)
library(mapview)
library(leaflet)
library(stringr)
library(leafsync)
library(leaflet.extras2)

# Define UI
ui <- fluidPage(
  titlePanel("Détection des évolutions des parcelles cadastrales (cas Vendée 2024-2023)"),
  
  # Navigation panel
  tabsetPanel(
    id = "tabsetPanel",  # Give an ID to the tabsetPanel
    type = "tabs",
    tabPanel("Comparaison par commune",
             fluidRow(
               column(12,
                      selectInput("nom_com_select", "Choisir un nom de commune:",
                                  choices = sort(unique(commune$nom_com)),
                                  selected = sort(unique(commune$nom_com))[1]),
                      hr(),
               )
             ),
             uiOutput("dynamicMaps")  # Use UI output to render maps
    ),
    tabPanel("Evolution sur le département entier",
             uiOutput("dynamicBigMap")
    )
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
  
  # Render maps dynamically
  output$dynamicMaps <- renderUI({
    req(input$tabsetPanel == "Comparaison par commune")
    
    map_1 <- mapview(bordure %>% 
                       filter(nom_com == input$nom_com_select), 
                     layer.name = "Bordures étendues", col.regions = "lightgrey", 
                     alpha.regions = 0.5, homebutton = F, 
                     map.types = c("CartoDB.Positron", "OpenStreetMap", "Esri.WorldImagery"))
    
    if (nrow(translation_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map_1 <- map_1 + mapview(translation_sql %>% 
                                 filter(nom_com == input$nom_com_select),
                               layer.name = paste0("Parcelles translatées (état 20",temps_apres,")"), 
                               col.regions = "darkcyan",
                               alpha.regions = 0.5, homebutton = F) +
        mapview(ins_parc_avant %>%
                  filter(idu %in% translation_sql$idu_translate) %>% 
                  filter(nom_com == input$nom_com_select), 
                col.regions = "darkcyan",
                layer.name = "Parcelles translatées (état 2023)", 
                alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(fusion_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map_1 <- map_1 + mapview(fusion_sql %>% 
                                 filter(nom_com == input$nom_com_select), 
                               layer.name = paste0("Parcelles fusionnéees (état 20",temps_apres,")"),
                               col.regions = "darkmagenta", alpha.regions = 0.5, homebutton = F) +
        mapview(supp_tot %>%
                  filter(idu %in% unlist(str_split(fusion_sql$participants, ",\\s*"))) %>% 
                  filter(nom_com == input$nom_com_select),  
                layer.name = paste0("Parcelles fusionnéees (état 20",temps_avant,")"), 
                col.regions = "darkmagenta",
                alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(subdiv_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map_1 <- map_1 + mapview(ajout_tot %>%
                                 filter(idu %in% unlist(str_split(subdiv_sql$participants, ",\\s*"))) %>% 
                                 filter(nom_com == input$nom_com_select),  
                               layer.name = paste0("Parcelles subdivisées (état 20",temps_apres,")"), 
                               col.regions = "purple",
                               alpha.regions = 0.5, homebutton = F) +
        mapview(subdiv_sql %>% 
                  filter(nom_com == input$nom_com_select),  
                layer.name = paste0("Parcelles subdivisées (état 20",temps_avant,")"), 
                col.regions = "purple", alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(multi_subdiv_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map_1 <- map_1 + mapview(ajout_tot %>%
                                 filter(idu %in% unlist(str_split(multi_subdiv_sql$participants_apres, ",\\s*"))) %>% 
                                 filter(nom_com == input$nom_com_select),
                               layer.name = paste0("Parcelles multi-subdivision (état 20",temps_apres,")"),
                               col.regions = "magenta",
                               alpha.regions = 0.5, homebutton = F) +
        mapview(multi_subdiv_sql %>% 
                  filter(nom_com == input$nom_com_select),  
                layer.name = paste0("Parcelles multi-subdivision (état 20",temps_avant,")"), 
                col.regions = "magenta", alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(contour_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map_1 <- map_1 + mapview(ajout_tot %>%
                                 filter(idu %in% unlist(str_split(contour_sql$participants_apres, ",\\s*"))) %>% 
                                 filter(nom_com == input$nom_com_select),
                               layer.name = paste0("Parcelles contours (état 20",temps_apres,")"), 
                               col.regions = "orange",
                               alpha.regions = 0.5, homebutton = F) +
        mapview(contour_sql %>% 
                  filter(nom_com == input$nom_com_select),
                layer.name = paste0("Parcelles contours (état 20",temps_avant,")"),
                col.regions = "orange", alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(contour_avant_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map_1 <- map_1 + mapview(contour_avant_sql %>% 
                                 filter(nom_com == input$nom_com_select),  
                               layer.name = paste0("Parcelles contours (état 20",temps_apres,")"),
                               col.regions = "orange",
                               alpha.regions = 0.5, homebutton = F) +
        mapview(ins_parc_apres %>%
                  filter(idu %in% contour_avant_sql$idu) %>% 
                  filter(nom_com == input$nom_com_select),  
                layer.name = paste0("Parcelles contours (état 20",temps_avant,")"), 
                col.regions = "orange", alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(contour_transfo_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map_1 <- map_1 + mapview(ajout_tot %>%
                                 filter(idu %in% unlist(str_split(contour_transfo_sql$participants_apres, ",\\s*"))) %>% 
                                 filter(nom_com == input$nom_com_select),  
                               layer.name = paste0("Parcelles transfo + contours (état 20",temps_apres,")"),
                               col.regions = "pink",
                               alpha.regions = 0.5, homebutton = F) +
        mapview(contour_transfo_sql %>% 
                  filter(nom_com == input$nom_com_select),
                layer.name = paste0("Parcelles transfo + contours (état 20",temps_avant,")"), 
                col.regions = "pink", alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(contour_translation_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map_1 <- map_1 + mapview(contour_translation_sql %>% 
                                 filter(nom_com == input$nom_com_select),  
                               layer.name = paste0("Parcelles translatées + contours (état 20",temps_apres,")"), 
                               alpha.regions = 0.5, homebutton = F) +
        mapview(ins_parc_avant %>%
                  filter(idu %in% contour_translation_sql$idu_translate) %>% 
                  filter(nom_com == input$nom_com_select),
                layer.name = paste0("Parcelles translatées + contours (état 20",temps_avant,")"), 
                alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(contour_transfo_translation_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map_1 <- map_1 + mapview(contour_fusion_translation_sql %>% 
                                 filter(nom_com == input$nom_com_select),  
                               layer.name = paste0("Parcelles transfo + translatées + contours (état 20",temps_apres,")"), 
                               col.regions = "lightblue",
                               alpha.regions = 0.5, homebutton = F) +
        mapview(ajout_tot %>%
                  filter(idu %in% contour_fusion_translation_sql$participants_apres_translate) %>% 
                  filter(nom_com == input$nom_com_select),
                layer.name = paste0("Parcelles transfo + translatées + contours (état 20",temps_avant,")"), 
                col.regions = "lightblue", alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(com_abs_apres_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map_1 <- map_1 + mapview(com_abs_apres_sql %>% 
                                 filter(nom_com == input$nom_com_select),  
                               layer.name = paste0("Parcelles fusion de communes (état 20",temps_apres,")"), 
                               col.regions = "lightgreen",
                               alpha.regions = 0.5, homebutton = F) +
        mapview(com_abs_avant_sql %>% 
                  filter(nom_com_apres == input$nom_com_select),  
                layer.name = paste0("Parcelles fusion de communes (état 20",temps_avant,")"), 
                col.regions = "lightgreen", alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(vrai_ajout_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map_1 <- map_1 + mapview(vrai_ajout_sql %>% 
                                 filter(nom_com == input$nom_com_select), 
                               layer.name = "Parcelles véritablement ajoutées", 
                               col.regions = "green", alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(vrai_supp_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map_1 <- map_1 + mapview(vrai_supp_sql %>% 
                                 filter(nom_com == input$nom_com_select),
                               layer.name = "Parcelles véritablement supprimées",
                               col.regions = "red", alpha.regions = 0.5, homebutton = F) 
      
    }
    if (nrow(ajout_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map_1 <- map_1 + mapview(ajout_sql %>% 
                                 filter(nom_com == input$nom_com_select),
                               z = c("iou_ajust"), layer.name = paste0("Parcelles restantes (état 20",temps_apres,")"), 
                               alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(supp_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map_1 <- map_1 + mapview(supp_sql %>% 
                                 filter(nom_com == input$nom_com_select), 
                               z = c("iou_multi"), layer.name = paste0("Parcelles restantes (état 20",temps_avant,")"), 
                               alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(modif_apres_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map_1 <- map_1 + mapview(modif_apres_sql %>% 
                                 filter(nom_com == input$nom_com_select), 
                               z = c("iou_ajust"), 
                               layer.name = paste0("Parcelles modifiées restantes (état 20",temps_apres,")"),
                               alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(modif_avant_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map_1 <- map_1 + mapview(modif_avant_sql %>% 
                                 filter(nom_com == input$nom_com_select), 
                               z = c("iou_ajust"), 
                               layer.name = paste0("Parcelles modifiées restantes (état 20",temps_avant,")"),
                               alpha.regions = 0.5, homebutton = F)
      
    }
    map_2 <- mapview(ins_parc_apres %>% 
                       filter(nom_com == input$nom_com_select), 
                     layer.name = paste0("Parcelles (état 20",temps_apres,")"),
                     col.regions = "purple", 
                     homebutton = FALSE) + 
      mapview(bordure %>% 
                filter(nom_com == input$nom_com_select), 
              layer.name = "Bordures étendues", 
              col.regions = "lightgrey", 
              alpha.regions = 0.5, 
              homebutton = FALSE)
    
    map_3 <- mapview(ins_parc_avant %>% 
                       filter(nom_com == input$nom_com_select), 
                     layer.name = paste0("Parcelles (état 20",temps_avant,")"), 
                     homebutton = FALSE) + 
      mapview(bordure %>% 
                filter(nom_com == input$nom_com_select), 
              layer.name = "Bordures étendues", 
              col.regions = "lightgrey", 
              alpha.regions = 0.5, legend = F,
              homebutton = FALSE)
    
    map_compa <- map_2 | map_3
    
    sync(map_1, map_compa@map, ncol = 1)
    
  })
  
  output$dynamicTwoMaps <- renderUI({
    req(input$tabsetPanel == "Evolution sur le département entier")
    fluidRow(
      column(12, leafletOutput("TwoMaps", height = "100vh"))
    )
  })
  
  output$dynamicBigMap <- renderUI({
    req(input$tabsetPanel == "Evolution sur le département entier")
    fluidRow(
      column(12, leafletOutput("largeMap", height = "100vh"))
    )
  })
  
  
  
  # Render the single large map
  output$largeMap <- renderLeaflet({
    req(input$tabsetPanel == "Evolution sur le département entier")
    map <- mapview(bordure, 
                   layer.name = "Bordures étendues", col.regions = "lightgrey", 
                   alpha.regions = 0.5, homebutton = F,
                   map.types = c("CartoDB.Positron", "OpenStreetMap", "Esri.WorldImagery"))
    
    if (nrow(translation_sql) > 0) {
      map <- map + mapview(translation_sql,
                           layer.name = paste0("Parcelles translatées (état 20",temps_apres,")"), 
                           col.regions = "darkcyan",
                           alpha.regions = 0.5, homebutton = F) +
        mapview(ins_parc_avant %>%
                  filter(idu %in% translation_sql$idu_translate),
                col.regions = "darkcyan",
                layer.name = paste0("Parcelles translatées (état 20",temps_avant,")"), 
                alpha.regions = 0.5, homebutton = F)
    }
    if (nrow(fusion_sql) > 0) {
      map <- map + mapview(fusion_sql, 
                           layer.name = paste0("Parcelles fusionnéees (état 20",temps_apres,")"),
                           col.regions = "darkmagenta", alpha.regions = 0.5, homebutton = F) +
        mapview(supp_tot %>%
                  filter(idu %in% unlist(str_split(fusion_sql$participants, ",\\s*"))),  
                layer.name = paste0("Parcelles fusionnéees (état 20",temps_avant,")"), 
                col.regions = "darkmagenta",
                alpha.regions = 0.5, homebutton = F)
    }
    if (nrow(subdiv_sql) > 0) {
      map <- map + mapview(ajout_tot %>%
                             filter(idu %in% unlist(str_split(subdiv_sql$participants, ",\\s*"))),  
                           layer.name = paste0("Parcelles subdivisées (état 20",temps_apres,")"), 
                           col.regions = "purple",
                           alpha.regions = 0.5, homebutton = F) +
        mapview(subdiv_sql,  
                layer.name = paste0("Parcelles subdivisées (état 20",temps_avant,")"), 
                col.regions = "purple", alpha.regions = 0.5, homebutton = F)
    }
    if (nrow(multi_subdiv_sql) > 0) {
      map <- map + mapview(ajout_tot %>%
                             filter(idu %in% unlist(str_split(multi_subdiv_sql$participants_apres, ",\\s*"))),
                           layer.name = paste0("Parcelles multi-subdivision (état 20",temps_apres,")"),
                           col.regions = "magenta",
                           alpha.regions = 0.5, homebutton = F) +
        mapview(multi_subdiv_sql,  
                layer.name = paste0("Parcelles multi-subdivision (état 20",temps_avant,")"), 
                col.regions = "magenta", alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(contour_sql) > 0) {
      map <- map + mapview(ajout_tot %>%
                             filter(idu %in% unlist(str_split(contour_sql$participants_apres, ",\\s*"))),  
                           layer.name = paste0("Parcelles contours (état 20",temps_apres,")"), 
                           col.regions = "orange",
                           alpha.regions = 0.5, homebutton = F) +
        mapview(contour_sql,
                layer.name = paste0("Parcelles contours (état 20",temps_avant,")"),
                col.regions = "orange", alpha.regions = 0.5, homebutton = F)
    }
    if (nrow(contour_avant_sql) > 0) {
      map <- map + mapview(contour_avant_sql,  
                           layer.name = paste0("Parcelles contours (état 20",temps_apres,")"),
                           col.regions = "orange",
                           alpha.regions = 0.5, homebutton = F) +
        mapview(ins_parc_avant %>%
                  filter(idu %in% contour_avant_sql$idu),  
                layer.name = paste0("Parcelles contours (état 20",temps_avant,")"), 
                col.regions = "orange", alpha.regions = 0.5, homebutton = F)
    }
    if (nrow(contour_transfo_sql) > 0) {
      map <- map + mapview(ajout_tot %>%
                             filter(idu %in% unlist(str_split(contour_transfo_sql$participants_apres, ",\\s*"))),  
                           layer.name = paste0("Parcelles transfo + contours (état 20",temps_apres,")"),
                           col.regions = "pink",
                           alpha.regions = 0.5, homebutton = F) +
        mapview(contour_transfo_sql,
                layer.name = paste0("Parcelles transfo + contours (état 20",temps_avant,")"), 
                col.regions = "pink", alpha.regions = 0.5, homebutton = F)
    }
    if (nrow(contour_translation_sql) > 0) {
      map <- map + mapview(contour_translation_sql,  
                           layer.name = paste0("Parcelles translatées + contours (état 20",temps_apres,")"), 
                           alpha.regions = 0.5, homebutton = F) +
        mapview(ins_parc_avant %>%
                  filter(idu %in% contour_translation_sql$idu_translate),
                layer.name = paste0("Parcelles translatées + contours (état 20",temps_avant,")"), 
                alpha.regions = 0.5, homebutton = F)
    }
    if (nrow(contour_transfo_translation_sql) > 0) {
      map <- map + mapview(contour_transfo_translation_sql,  
                           layer.name = paste0("Parcelles transfo + translatées + contours (état 20",temps_apres,")"), 
                           col.regions = "lightblue",
                           alpha.regions = 0.5, homebutton = F) +
        mapview(ajout_tot %>%
                  filter(idu %in% contour_transfo_translation_sql$participants_apres_translate),
                layer.name = paste0("Parcelles transfo + translatées + contours (état 20",temps_avant,")"), 
                col.regions = "lightblue", alpha.regions = 0.5, homebutton = F)
    }
    if (nrow(com_abs_apres_sql) > 0) {
      map <- map + mapview(com_abs_apres_sql,  
                           layer.name = paste0("Parcelles fusion de communes (état 20",temps_apres,")"), 
                           col.regions = "lightgreen",
                           alpha.regions = 0.5, homebutton = F) +
        mapview(com_abs_avant_sql,  
                layer.name = paste0("Parcelles fusion de communes (état 20",temps_avant,")"), 
                col.regions = "lightgreen", alpha.regions = 0.5, homebutton = F)
    }
    if (nrow(vrai_ajout_sql) > 0) {
      map <- map + mapview(vrai_ajout_sql, 
                           layer.name = "Parcelles véritablement ajoutées", 
                           col.regions = "green", alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(vrai_supp_sql) > 0) {
      map <- map + mapview(vrai_supp_sql,
                           layer.name = "Parcelles véritablement supprimées",
                           col.regions = "red", alpha.regions = 0.5, homebutton = F) 
    }
    if (nrow(ajout_sql) > 0) {
      map <- map + mapview(ajout_sql,
                           z = c("iou_ajust"), layer.name = paste0("Parcelles restantes (état 20",temps_apres,")"), 
                           alpha.regions = 0.5, homebutton = F)
    }
    if (nrow(supp_sql) > 0) {
      map <- map + mapview(supp_sql, 
                           z = c("iou_multi"), layer.name = paste0("Parcelles restantes (état 20",temps_avant,")"), 
                           alpha.regions = 0.5, homebutton = F)
    }
    if (nrow(modif_apres_sql) > 0) {
      map <- map + mapview(modif_apres_sql, 
                           z = c("iou_ajust"), 
                           layer.name = paste0("Parcelles modifiées restantes (état 20",temps_apres,")"), 
                           alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(modif_avant_sql) > 0) {
      map <- map + mapview(modif_avant_sql, 
                           z = c("iou_ajust"), 
                           layer.name = paste0("Parcelles modifiées restantes (état 20",temps_avant,")"), 
                           alpha.regions = 0.5, homebutton = F)
    }
    map@map
  })
  
}

# Run the application
shinyApp(ui, server)
