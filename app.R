library(shiny)
library(sf)
library(mapview)
library(leaflet)
library(stringr)


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
                                  choices = unique(ins_parc_apres$nom_com),
                                  selected = unique(ins_parc_apres$nom_com)[1]),
                      actionButton("resetButton", "Réinitialiser la sélection"),
                      actionButton("syncButton", "Synchronisation des cartes"),
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
    fluidRow(
      # Colonne gauche pour la carte 1 (en haut au milieu)
      column(12, leafletOutput("map1", height = "58vh")),
      
      # Colonne droite pour les cartes 2 et 3 (en bas)
      column(12, 
             fluidRow(
               # Colonne gauche pour la carte 2 (en bas à gauche)
               column(6, leafletOutput("map2", height = "58vh")),
               
               # Colonne droite pour la carte 3 (en bas à droite)
               column(6, leafletOutput("map3", height = "58vh"))
             )
      )
    )
  })
  output$dynamicBigMap <- renderUI({
    req(input$tabsetPanel == "Evolution sur le département entier")
    fluidRow(
      column(12, leafletOutput("largeMap", height = "100vh"))
    )
  })
  
  # Render the three maps only when the tab is active
  output$map1 <- renderLeaflet({
    req(input$tabsetPanel == "Comparaison par commune")
    map <- mapview(commune %>% 
                     filter(nom_com == input$nom_com_select), 
                   layer.name = "Communes", col.regions = "white", 
                   alpha.regions = 0.5, homebutton = F) +
      mapview(bordure %>% 
                filter(nom_com == input$nom_com_select), 
              layer.name = "Bordures étendues", col.regions = "lightblue", 
              alpha.regions = 0.5, homebutton = F)
    
    
    if (nrow(translation_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map <- map + mapview(translation_sql %>% 
                             filter(nom_com == input$nom_com_select),
                           layer.name = "Parcelles translatées (état 2024)", 
                           alpha.regions = 0.5, homebutton = F)
    }
    if (nrow(translation_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map <- map + mapview(translation_sql %>% 
                             filter(nom_com == input$nom_com_select),
                           layer.name = "Parcelles translatées (état 2024)", 
                           alpha.regions = 0.5, homebutton = F) +
        mapview(ins_parc_avant %>%
                  filter(idu %in% translation_sql$idu_translate) %>% 
                  filter(nom_com == input$nom_com_select), 
                layer.name = "Parcelles translatées (état 2023)", 
                alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(fusion_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map <- map + mapview(fusion_sql %>% 
                             filter(nom_com == input$nom_com_select), 
                           layer.name = "Parcelles fusionnéees (état 2024)", 
                           col.regions = "orange", alpha.regions = 0.5, homebutton = F) +
        mapview(supp_tot %>%
                  filter(idu %in% unlist(str_split(fusion_sql$participants, ",\\s*"))) %>% 
                  filter(nom_com == input$nom_com_select),  
                layer.name = "Parcelles avant fusion (état 2023)", 
                alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(subdiv_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map <- map + mapview(ajout_tot %>%
                             filter(idu %in% unlist(str_split(subdiv_sql$participants, ",\\s*"))) %>% 
                             filter(nom_com == input$nom_com_select),  
                           layer.name = "Parcelles subdivisées (état 2024)", 
                           alpha.regions = 0.5, homebutton = F) +
        mapview(subdiv_sql %>% 
                  filter(nom_com == input$nom_com_select),  
                layer.name = "Parcelles avant subdivision (état 2023)", 
                col.regions = "orange", alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(multi_subdiv_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map <- map + mapview(ajout_tot %>%
                             filter(idu %in% unlist(str_split(multi_subdiv_sql$participants_apres, ",\\s*"))) %>% 
                             filter(nom_com == input$nom_com_select),
                           layer.name = "Parcelles après multi-subdivision (état 2024)", 
                           alpha.regions = 0.5, homebutton = F) +
        mapview(multi_subdiv_sql %>% 
                  filter(nom_com == input$nom_com_select),  
                layer.name = "Parcelles avant multi-subdivision (état 2023)", 
                col.regions = "orange", alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(contour_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map <- map + mapview(ajout_tot %>%
                             filter(idu %in% unlist(str_split(contour_sql$participants_apres, ",\\s*"))) %>% 
                             filter(nom_com == input$nom_com_select),  
                           layer.name = "Parcelles après évolution forme (état 2024)", 
                           alpha.regions = 0.5, homebutton = F) +
        mapview(contour_sql %>% 
                  filter(nom_com == input$nom_com_select),
                layer.name = "Parcelles avant évolution forme (état 2023)",
                col.regions = "orange", alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(contour_apres_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map <- map + mapview(contour_apres_sql %>% 
                             filter(nom_com == input$nom_com_select),  
                           layer.name = "Parcelles après évolution forme (état 2024)", 
                           alpha.regions = 0.5, homebutton = F) +
        mapview(ins_parc_avant %>%
                  filter(idu %in% contour_apres_sql$idu) %>% 
                  filter(nom_com == input$nom_com_select),  
                layer.name = "Parcelles avant évolution forme (état 2023)", 
                col.regions = "orange", alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(contour_transfo_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map <- map + mapview(ajout_tot %>%
                             filter(idu %in% unlist(str_split(contour_transfo_sql$participants_apres, ",\\s*"))) %>% 
                             filter(nom_com == input$nom_com_select),  
                           layer.name = "Parcelles ayant transfo + évolution forme (état 2024)",
                           alpha.regions = 0.5, homebutton = F) +
        mapview(contour_transfo_sql %>% 
                  filter(nom_com == input$nom_com_select),
                layer.name = "Parcelles ayant transfo + évolution forme (état 2023)", 
                col.regions = "orange", alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(contour_translation_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map <- map + mapview(contour_translation_sql %>% 
                             filter(nom_com == input$nom_com_select),  
                           layer.name = "Parcelles ayant translatées + évolution forme (état 2024)", 
                           alpha.regions = 0.5, homebutton = F) +
        mapview(ins_parc_avant %>%
                  filter(idu %in% contour_translation_sql$idu_translate) %>% 
                  filter(nom_com == input$nom_com_select),
                layer.name = "Parcelles ayant translatées + évolution forme (état 2023)", 
                col.regions = "orange", alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(contour_fusion_translation_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map <- map + mapview(contour_fusion_translation_sql %>% 
                             filter(nom_com == input$nom_com_select),  
                           layer.name = "Parcelles ayant translatées + évolution forme + fusion (état 2024)", 
                           alpha.regions = 0.5, homebutton = F) +
        mapview(ajout_tot %>%
                  filter(idu %in% contour_fusion_translation_sql$participants_apres_multi_translate) %>% 
                  filter(nom_com == input$nom_com_select),
                layer.name = "Parcelles ayant translatées + évolution forme + fusion (état 2023)", 
                col.regions = "orange", alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(com_abs_apres_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map <- map + mapview(com_abs_apres_sql %>% 
                             filter(nom_com == input$nom_com_select),  
                           layer.name = "Parcelles après fusion de communes (état 2024)", 
                           col.regions = "pink",
                           alpha.regions = 0.5, homebutton = F) +
        mapview(com_abs_avant_sql %>% 
                  filter(nom_com_apres == input$nom_com_select),  
                layer.name = "Parcelles avant fusion de communes (état 2023)", 
                col.regions = "pink", alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(vrai_ajout_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map <- map + mapview(vrai_ajout_sql %>% 
                             filter(nom_com == input$nom_com_select), 
                           layer.name = "Parcelles véritablement ajoutées", 
                           col.regions = "green", alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(vrai_supp_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map <- map + mapview(vrai_supp_sql %>% 
                             filter(nom_com == input$nom_com_select),
                           layer.name = "Parcelles véritablement supprimées",
                           col.regions = "red", alpha.regions = 0.5, homebutton = F) 
      
    }
    if (nrow(ajout_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map <- map + mapview(ajout_sql %>% 
                             filter(nom_com == input$nom_com_select),
                           z = c("iou_ajust"), layer.name = "Parcelles restantes (état 2024)", 
                           alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(supp_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map <- map + mapview(supp_sql %>% 
                             filter(nom_com == input$nom_com_select), 
                           z = c("iou_multi"), layer.name = "Parcelles restantes (état 2023)", 
                           alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(modif_apres_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map <- map + mapview(modif_apres_sql %>% 
                             filter(nom_com == input$nom_com_select), 
                           z = c("iou_ajust"), 
                           layer.name = "Parcelles modifiées restantes (état 2024)", 
                           alpha.regions = 0.5, homebutton = F)
      
    }
    if (nrow(modif_avant_sql %>% filter(nom_com == input$nom_com_select)) > 0) {
      map <- map + mapview(modif_avant_sql %>% 
                             filter(nom_com == input$nom_com_select), 
                           z = c("iou_ajust"), 
                           layer.name = "Parcelles modifiées restantes (état 2023)", 
                           alpha.regions = 0.5, homebutton = F)
      
    }
    map@map
  })
  
  output$map2 <- renderLeaflet({
    req(input$tabsetPanel == "Comparaison par commune")
    (mapview(ins_parc_avant %>% 
               filter(nom_com == input$nom_com_select), 
             layer.name = "Parcelles (état 2023)", 
             homebutton = FALSE) + 
        mapview(bordure %>% 
                  filter(nom_com == input$nom_com_select), 
                layer.name = "Bordures étendues", 
                col.regions = "lightblue", 
                alpha.regions = 0.5, 
                homebutton = FALSE))@map
  })
  
  output$map3 <- renderLeaflet({
    req(input$tabsetPanel == "Comparaison par commune")
    (mapview(ins_parc_apres %>% 
               filter(nom_com == input$nom_com_select), 
             layer.name = "Parcelles (état 2024)",
             col.regions = "purple", 
             homebutton = FALSE) + 
        mapview(bordure %>% 
                  filter(nom_com == input$nom_com_select), 
                layer.name = "Bordures étendues", 
                col.regions = "lightblue", 
                alpha.regions = 0.5, 
                homebutton = FALSE))@map
  })
  
  # Render the single large map
  output$largeMap <- renderLeaflet({
    req(input$tabsetPanel == "Evolution sur le département entier")
    (mapview(commune, layer.name = "Communes", col.regions = "white", 
             alpha.regions = 0.5, homebutton = F) +
       mapview(bordure, layer.name = "Bordures étendues", col.regions = "lightblue", 
               alpha.regions = 0.5, homebutton = F) +
       mapview(translation_sql,
               layer.name = "Parcelles translatées (état 2024)", 
               alpha.regions = 0.5, homebutton = F) +
       mapview(ins_parc_avant %>%
                 filter(idu %in% translation_sql$idu_translate), 
               layer.name = "Parcelles translatées (état 2023)", 
               alpha.regions = 0.5, homebutton = F) +
       mapview(fusion_sql, layer.name = "Parcelles fusionnéees (état 2024)", 
               col.regions = "orange", alpha.regions = 0.5, homebutton = F) +
       mapview(supp_tot %>%
                 filter(idu %in% unlist(str_split(fusion_sql$participants, ",\\s*"))),  
               layer.name = "Parcelles avant fusion (état 2023)", 
               alpha.regions = 0.5, homebutton = F) +
       mapview(ajout_tot %>%
                 filter(idu %in% unlist(str_split(subdiv_sql$participants, ",\\s*"))),  
               layer.name = "Parcelles subdivisées (état 2024)", 
               alpha.regions = 0.5, homebutton = F) +
       mapview(subdiv_sql,  layer.name = "Parcelles avant subdivision (état 2023)", 
               col.regions = "orange", alpha.regions = 0.5, homebutton = F) +
       mapview(ajout_tot %>%
                 filter(idu %in% unlist(str_split(multi_subdiv_sql$participants_apres, ",\\s*"))),
               layer.name = "Parcelles après multi-subdivision (état 2024)", 
               alpha.regions = 0.5, homebutton = F) +
       mapview(multi_subdiv_sql,  
               layer.name = "Parcelles avant multi-subdivision (état 2023)", 
               col.regions = "orange", alpha.regions = 0.5, homebutton = F) +
       mapview(ajout_tot %>%
                 filter(idu %in% unlist(str_split(contour_sql$participants_apres, ",\\s*"))),  
               layer.name = "Parcelles après évolution forme (état 2024)", 
               alpha.regions = 0.5, homebutton = F) +
       mapview(contour_sql,
               layer.name = "Parcelles avant évolution forme (état 2023)",
               col.regions = "orange", alpha.regions = 0.5, homebutton = F) +
       mapview(contour_apres_sql,  
               layer.name = "Parcelles après évolution forme (état 2024)", 
               alpha.regions = 0.5, homebutton = F) +
       mapview(ins_parc_avant %>%
                 filter(idu %in% contour_apres_sql$idu),  
               layer.name = "Parcelles avant évolution forme (état 2023)", 
               col.regions = "orange", alpha.regions = 0.5, homebutton = F) +
       mapview(ajout_tot %>%
                 filter(idu %in% unlist(str_split(contour_transfo_sql$participants_apres, ",\\s*"))),  
               layer.name = "Parcelles ayant transfo + évolution forme (état 2024)",
               alpha.regions = 0.5, homebutton = F) +
       mapview(contour_transfo_sql,
               layer.name = "Parcelles ayant transfo + évolution forme (état 2023)", 
               col.regions = "orange", alpha.regions = 0.5, homebutton = F) +
       mapview(contour_translation_sql,  
               layer.name = "Parcelles ayant translatées + évolution forme (état 2024)", 
               alpha.regions = 0.5, homebutton = F) +
       mapview(ins_parc_avant %>%
                 filter(idu %in% contour_translation_sql$idu_translate),
               layer.name = "Parcelles ayant translatées + évolution forme (état 2023)", 
               col.regions = "orange", alpha.regions = 0.5, homebutton = F) +
       mapview(contour_fusion_translation_sql,  
               layer.name = "Parcelles ayant translatées + évolution forme + fusion (état 2024)", 
               alpha.regions = 0.5, homebutton = F) +
       mapview(ajout_tot %>%
                 filter(idu %in% contour_fusion_translation_sql$participants_apres_multi_translate),
               layer.name = "Parcelles ayant translatées + évolution forme + fusion (état 2023)", 
               col.regions = "orange", alpha.regions = 0.5, homebutton = F) +
       mapview(com_abs_apres_sql,  layer.name = "Parcelles après fusion de communes (état 2024)", 
               col.regions = "pink", alpha.regions = 0.5, homebutton = F) +
       mapview(com_abs_avant_sql,  layer.name = "Parcelles avant fusion de communes (état 2023)", 
               col.regions = "pink", alpha.regions = 0.5, homebutton = F) +
       mapview(vrai_ajout_sql, layer.name = "Parcelles véritablement ajoutées", 
               col.regions = "green", alpha.regions = 0.5, homebutton = F) +
       mapview(vrai_supp_sql, layer.name = "Parcelles véritablement supprimées",
               col.regions = "red", alpha.regions = 0.5, homebutton = F) +
       mapview(ajout_sql, z = c("iou_ajust"), layer.name = "Parcelles restantes (état 2024)", 
               alpha.regions = 0.5, homebutton = F) +
       mapview(supp_sql, z = c("iou_multi"), layer.name = "Parcelles restantes (état 2023)", 
               alpha.regions = 0.5, homebutton = F) +
       mapview(modif_apres_sql, z = c("iou_ajust"), 
               layer.name = "Parcelles modifiées restantes (état 2024)", 
               alpha.regions = 0.5, homebutton = F) +
       mapview(modif_avant_sql, z = c("iou_ajust"), 
               layer.name = "Parcelles modifiées restantes (état 2023)", 
               alpha.regions = 0.5, homebutton = F)
    )@map
  })
  
  # Synchronize maps
  observeEvent(input$syncButton, {
    session$sendCustomMessage("syncMaps", list(mapId1 = "map1", mapId2 = "map2", mapId3 = "map3"))
  })
  
  # Reset selection to first element
  observeEvent(input$resetButton, {
    updateSelectInput(session, "nom_com_select", selected = unique(ins_parc_apres$nom_com)[1])
  })
}

# JavaScript for synchronizing maps
jsCode <- "
Shiny.addCustomMessageHandler('syncMaps', function(message) {
  var map1 = $('#' + message.mapId1).data('leaflet-map');
  var map2 = $('#' + message.mapId2).data('leaflet-map');
  var map3 = $('#' + message.mapId3).data('leaflet-map');
  if (map1 && map2 && map3) {
    map2.setView(map1.getCenter(), map1.getZoom(), { animate: true });
    map3.setView(map1.getCenter(), map1.getZoom(), { animate: true });
  }
});
"

# Include the synchronization script in the head
syncLeafletScript <- tags$head(
  tags$script(HTML(jsCode))
)

# Add the synchronization script to the app
ui <- tagList(syncLeafletScript, ui)

# Run the application
shinyApp(ui, server)
