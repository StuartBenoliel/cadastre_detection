library(shiny)
library(sf)
library(mapview)
library(leaflet)

parcelles_avant <- ins_parc_23
parcelles_apres <- ins_parc_24

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
                                  choices = unique(parcelles_apres$nom_com),
                                  selected = unique(parcelles_apres$nom_com)[1]),
                      actionButton("resetButton", "Réinitialiser la sélection"),
                      actionButton("syncButton", "Synchronisation des cartes"),
                      hr(),
               )
             ),
             uiOutput("dynamicMaps")  # Use UI output to render maps
    ),
    tabPanel("Evolution sur le département entier",
             fluidRow(
               column(12, leafletOutput("largeMap", height = "100vh"))
             )
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
  
  parcelles_avant_f <- reactive({
    parcelles_avant[parcelles_avant$nom_com == input$nom_com_select, ]
  })
  
  parcelles_apres_f <- reactive({
    parcelles_apres[parcelles_apres$nom_com == input$nom_com_select, ]
  })
  
  bordure_f <- reactive({
    bordure[bordure$nom_com == input$nom_com_select, ]
  })
  
  
  # Render maps dynamically
  output$dynamicMaps <- renderUI({
    req(input$tabsetPanel == "Comparaison par commune")
    fluidRow(
      column(4, leafletOutput("map2", height = "84vh")),
      column(4, leafletOutput("map1", height = "84vh")),
      column(4, leafletOutput("map3", height = "84vh"))
    )
  })
  
  # Render the three maps only when the tab is active
  output$map1 <- renderLeaflet({
    req(input$tabsetPanel == "Comparaison par commune")
    (mapview(parcelles_avant_f(), homebutton = FALSE) + 
        mapview(bordure_f(), 
                layer.name = "Bordures étendues", 
                col.regions = "lightblue", 
                alpha.regions = 0.5, 
                homebutton = FALSE))@map
  })
  
  output$map2 <- renderLeaflet({
    req(input$tabsetPanel == "Comparaison par commune")
    (mapview(parcelles_avant_f(), 
             layer.name = "Parcelles (état 2023)", 
             homebutton = FALSE) + 
        mapview(bordure_f(), 
                layer.name = "Bordures étendues", 
                col.regions = "lightblue", 
                alpha.regions = 0.5, 
                homebutton = FALSE))@map
  })
  
  output$map3 <- renderLeaflet({
    req(input$tabsetPanel == "Comparaison par commune")
    (mapview(parcelles_apres_f(), 
             layer.name = "Parcelles (état 2024)",
             col.regions = "purple", 
             homebutton = FALSE) + 
        mapview(bordure_f(), 
                layer.name = "Bordures étendues", 
                col.regions = "lightblue", 
                alpha.regions = 0.5, 
                homebutton = FALSE))@map
  })
  
  # Render the single large map
  output$largeMap <- renderLeaflet({
    req(input$tabsetPanel == "Evolution sur le département entier")
    mapview(commune, homebutton = FALSE)@map
  })
  
  # Synchronize maps
  observeEvent(input$syncButton, {
    session$sendCustomMessage("syncMaps", list(mapId1 = "map1", mapId2 = "map2", mapId3 = "map3"))
  })
  
  # Reset selection to first element
  observeEvent(input$resetButton, {
    updateSelectInput(session, "nom_com_select", selected = unique(parcelles_apres$nom_com)[1])
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
