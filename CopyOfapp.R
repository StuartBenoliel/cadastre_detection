library(shiny)
library(sf)
library(mapview)
library(leaflet)
library(stringr)
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
                                  choices = unique(commune$nom_com),
                                  selected = unique(commune$nom_com)[1]),
                      hr()
               )
             ),
             
             uiOutput("dynamicMaps", style = "width: 102%;"))  
    
  ),
)

# Define server logic
server <- function(input, output, session) {
  
  # Render maps dynamically
  output$dynamicMaps <- renderUI({
    req(input$tabsetPanel == "Comparaison par commune")
    
    # Generate maps
    map1 <- mapview(ins_parc_avant %>% 
                      filter(nom_com == input$nom_com_select), 
                    layer.name = "Parcelles (état 2023)", 
                    homebutton = FALSE)@map
    map2 <- mapview(bordure %>% 
                      filter(nom_com == input$nom_com_select), 
                    layer.name = "Bordures étendues", 
                    col.regions = "lightblue", 
                    alpha.regions = 0.5, 
                    homebutton = FALSE)
    map3 <- mapview(ins_parc_avant %>% 
                      filter(nom_com == input$nom_com_select), 
                    layer.name = "Parcelles (état 2023)", 
                    homebutton = FALSE)
    
    map <- map2 | map3
    sync(map1, map@map, ncol = )
  })
  
}
# Run the application
shinyApp(ui, server)
