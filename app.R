library(shiny)
library(sf)
library(mapview)
library(leaflet)

# Example data
sample_data <- st_as_sf(data.frame(
  id = 1:3,
  nom_com = c("Ville A", "Ville B", "Ville C"),
  geometry = st_sfc(
    st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    st_polygon(list(rbind(c(1, 1), c(2, 1), c(2, 2), c(1, 2), c(1, 1)))),
    st_polygon(list(rbind(c(0, 1), c(0, 2), c(1, 2), c(1, 1), c(0, 1))))
  )
), crs = 4326)

# Replace with your actual data
sample_data <- ins_parc_23
sample_data_2 <- ins_parc_24

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
                                  choices = unique(sample_data$nom_com),
                                  selected = unique(sample_data$nom_com)[1]),
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
  "))
)

# Define server logic
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    sample_data[sample_data$nom_com == input$nom_com_select, ]
  })
  
  filtered_data_2 <- reactive({
    sample_data_2[sample_data_2$nom_com == input$nom_com_select, ]
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
    mapview(filtered_data())@map
  })
  
  output$map2 <- renderLeaflet({
    req(input$tabsetPanel == "Comparaison par commune")
    mapview(filtered_data(), 
            layer.name = "Parcelles (état 2023)",
            col.regions = "pink")@map
  })
  
  output$map3 <- renderLeaflet({
    req(input$tabsetPanel == "Comparaison par commune")
    mapview(filtered_data_2(), 
            layer.name = "Parcelles (état 2024)",
            col.regions = "purple")@map
  })
  
  # Render the single large map
  output$largeMap <- renderLeaflet({
    req(input$tabsetPanel == "Evolution sur le département entier")
    mapview(filtered_data())@map
  })
  
  # Synchronize maps
  observeEvent(input$syncButton, {
    session$sendCustomMessage("syncMaps", list(mapId1 = "map1", mapId2 = "map2", mapId3 = "map3"))
  })
  
  # Reset selection to first element
  observeEvent(input$resetButton, {
    updateSelectInput(session, "nom_com_select", selected = unique(sample_data$nom_com)[1])
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
