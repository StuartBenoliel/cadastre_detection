library(shiny)
library(sf)
library(mapview)
library(leaflet)

# Exemple de données
sample_data <- st_as_sf(data.frame(
  id = 1:3,
  nom_com = c("Ville A", "Ville B", "Ville C"),
  geometry = st_sfc(
    st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    st_polygon(list(rbind(c(1, 1), c(2, 1), c(2, 2), c(1, 2), c(1, 1)))),
    st_polygon(list(rbind(c(0, 1), c(0, 2), c(1, 2), c(1, 1), c(0, 1))))
  )
), crs = 4326)

sample_data <- commune

# Définir l'interface utilisateur
ui <- fluidPage(
  titlePanel("Filtered Polygon Map in Shiny"),
  fluidRow(
    column(12,
           h4("Filter Polygons by nom_com"),
           selectInput("nom_com_select", "Choose nom_com:",
                       choices = c("", unique(sample_data$nom_com))),
           actionButton("resetButton", "Reset Selection"),
           actionButton("syncButton", "Synchronize Maps"),
           hr(),
           p("This app filters polygons based on the selected nom_com.")
    )
  ),
  fluidRow(
    column(4,
           leafletOutput("map1", height = 400)
    ),
    column(4,
           leafletOutput("map2", height = 400)
    ),
    column(4,
           leafletOutput("map3", height = 400)
    )
  )
)

# Définir la logique serveur
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    if (input$nom_com_select == "") {
      return(sample_data)  # Retourner toutes les données si aucun filtre sélectionné
    } else {
      return(sample_data[sample_data$nom_com == input$nom_com_select, ])
    }
  })
  
  # Rendre les objets mapview
  output$map1 <- renderLeaflet({
    mapview(filtered_data())@map
  })
  
  output$map2 <- renderLeaflet({
    mapview(filtered_data())@map
  })
  
  output$map3 <- renderLeaflet({
    mapview(filtered_data())@map
  })
  
  # Observer pour synchroniser les cartes après le rendu initial
  observeEvent(input$syncButton, {
    session$sendCustomMessage("syncMaps", list(mapId1 = "map1", mapId2 = "map2", mapId3 = "map3"))
  })
  
  # Réinitialiser la sélection de nom_com
  observeEvent(input$resetButton, {
    updateSelectInput(session, "nom_com_select", selected = "")
  })
  
}

# JavaScript pour synchroniser les cartes
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

# Inclure le script de synchronisation leaflet dans la balise head
syncLeafletScript <- tags$head(
  tags$script(HTML(jsCode))
)

# Ajouter le script de synchronisation à l'application
ui <- tagList(syncLeafletScript, ui)

# Exécuter l'application
shinyApp(ui, server)