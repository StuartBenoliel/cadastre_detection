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
source(file = "fonction_shiny.R")

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
                      uiOutput("temps_select_ui")
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
                      uiOutput("temps_select_ui")
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
  
  # Fonction pour mettre à jour les sélecteurs
  updateSelectors <- function(depart_select, temps_select) {
    num_departement <<- depart_select
    print(paste0("Changement au niveau du département: ", depart_select))
    commune <- load_communes(conn, depart_select)
    
    updateSelectInput(session, "nom_com_select",
                      choices = sort(unique(commune$nom_com)),
                      selected = sort(unique(commune$nom_com))[1])
    
    result <- dbGetQuery(conn, paste0(" 
      SELECT 
          schema_name
      FROM 
          information_schema.schemata
      WHERE 
          schema_name LIKE 'traitement_%_%_cadastre_", depart_select,"';
               "))
    
    int_temps <- result$schema_name %>%
      str_extract("traitement_(\\d{2})_(\\d{2})_cadastre") %>%
      str_replace_all("traitement_(\\d{2})_(\\d{2})_cadastre", "\\1-\\2") %>%
      as.character()
    
    int_temps <- int_temps[order(as.numeric(str_extract(int_temps, "^[0-9]+")) , decreasing = TRUE)]
    
    output$temps_select_ui <- renderUI({
      selectInput("temps_select", "Choisir une période de temps:",
                  choices = int_temps,
                  selected = int_temps[1])
    })
    
  }
  
  observeEvent(input$depart_select, {
    updateSelectors(input$depart_select, input$temps_select)
  })
  
  temps_reactive <- reactive({
    print(paste0("Changement au niveau de la période de temps: ", input$temps_select))
    temps_split <- strsplit(input$temps_select, "-")[[1]]
    temps_apres <<- temps_split[1]
    temps_avant <<- temps_split[2]
    update_search_path(conn, input$depart_select, temps_apres, temps_avant)
  })
  
  output$dynamicMaps <- renderUI({
    req(input$tabsetPanel == "Carte des comparaison par commune")
    req(input$temps_select)
    isolate({
      temps_reactive()
    })
    print(paste0("Affichage des cartes pour la commune: ", input$nom_com_select))
    nom_com_select <- gsub("'", "''", input$nom_com_select)
    cartes_dynamiques(conn, num_departement, temps_apres, temps_avant, nom_com_select)
  })
  
  output$table <- renderDT({
    req(input$tabsetPanel == "Liste des changments par département")
    req(input$temps_select)
    isolate({
      temps_reactive()  
    })
    print(paste0("Affichage de la liste des changements au niveau du département: ", num_departement))
    tableau_recap(conn, input$depart_select, temps_apres, temps_avant)
  })
  
  observeEvent(input$tabsetPanel, {
    if (input$tabsetPanel == "Carte des comparaison par commune" || input$tabsetPanel == "Liste des changments par département") {
      updateSelectors(input$depart_select, input$temps_select)
    }
  })
}

# defusion fusion
shinyApp(ui, server)
