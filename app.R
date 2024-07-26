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
source(file = "fonction_shiny.R")
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
    tabPanel("Carte des comparaisons par commune",
             br(),
             fluidRow(
               column(3,
                      selectInput("depart_select_carte", "Choisir un département:",
                                  choices = departements,
                                  selected = departements[length(departements)])
               ),
               column(3,
                      selectInput("temps_select_carte", "Choisir une période de temps:",
                                  choices = NULL,
                                  selected = NULL)
               ),
               column(3,
                      selectInput("nom_com_select_carte", "Choisir un nom de commune:",
                                  choices = NULL,
                                  selected = NULL)
               ),
             ),
             uiOutput("dynamicMaps")  # Use UI output to render maps
    ),
    tabPanel("Tableau des changments par commune",
             br(),
             fluidRow(
               column(3,
                      selectInput("depart_select_tableau", "Choisir un département:",
                                  choices = departements,
                                  selected = departements[length(departements)])
               ),
               column(3,
                      selectInput("temps_select_tableau", "Choisir une période de temps:",
                                  choices = NULL,
                                  selected = NULL)
               ),
             ),
             DTOutput("table"),
             DTOutput("changement_commune")
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
  
  observeEvent(input$tabsetPanel, {
    req(input$temps_select_carte) # Evite que cela se lance au démarage
    print("Changement d'onglet")
    if (input$tabsetPanel == "Carte des comparaisons par commune") {
      update_search_path(conn, input$depart_select_carte, temps_vec_carte[1], temps_vec_carte[2])
    }
  })
  
  # Lancement au démarage
  observeEvent(input$depart_select_carte, {
    num_departement <<- input$depart_select_carte
    print(paste0("Changement au niveau du département carte: ", num_departement))
    commune <- load_communes(conn, num_departement)
    
    updateSelectInput(session, "nom_com_select_carte",
                      choices = sort(unique(commune$nom_com)),
                      selected = sort(unique(commune$nom_com))[1])
    
    
    int_temps <- intervalle_temps(conn, num_departement)
    
    updateSelectInput(session, "temps_select_carte",
                      choices = int_temps,
                      selected = int_temps[1])
    
  })

  temps_reactive <- reactive({
    print(paste0("Changement au niveau de la période de temps carte: ", input$temps_select_carte))
    temps_vec_carte <<- strsplit(input$temps_select_carte, "-")[[1]]
    update_search_path(conn, num_departement, temps_vec_carte[1], temps_vec_carte[2])
  })
  
  # Rendu dynamique des cartes
  output$dynamicMaps <- renderUI({
    req(input$tabsetPanel == "Carte des comparaisons par commune")
    req(input$temps_select_carte) # Permet de relancer lors d'un changement de temps
    # Assurer que temps_reactive est terminé avant de continuer
    isolate({
      temps_reactive()
    })
    print(paste0("Affichage des cartes pour la commune: ", input$nom_com_select_carte))
    nom_com <- gsub("'", "''", input$nom_com_select_carte)
    
    cartes_dynamiques(conn, num_departement, temps_vec_carte[1], temps_vec_carte[2], nom_com)
  })
  
  # Lancement au démarage
  observeEvent(input$depart_select_tableau, {
    print(paste0("Changement au niveau du département tableau: ", input$depart_select_tableau))
    int_temps <- intervalle_temps(conn, input$depart_select_tableau)
    temps_vec_tableau <<- strsplit(int_temps[1], "-")[[1]]

    updateSelectInput(session, "temps_select_tableau",
                      choices = int_temps,
                      selected = int_temps[1])
  })
  
  observeEvent(input$temps_select_tableau, {
    req(input$tabsetPanel == "Tableau des changments par commune")
    print(paste0("Changement au niveau de la période de temps tableau: ", input$temps_select_tableau))
    temps_vec_tableau <<- strsplit(input$temps_select_tableau, "-")[[1]]
  })

  output$table <- renderDT({
    req(input$tabsetPanel == "Tableau des changments par commune")
    req(input$temps_select_tableau) # Permet de relancer lors d'un changement de temps
    # Exécuter la mise à jour du chemin de recherche uniquement après le changement de département et de période
    isolate({
      update_search_path(conn, input$depart_select_tableau, temps_vec_tableau[1], temps_vec_tableau[2])
    })
    print(paste0("Affichage du tableau des changements au niveau du département: ", input$depart_select_tableau))
    tableau_recap(conn, input$depart_select_tableau, temps_vec_tableau[1], temps_vec_tableau[2])
  })
  output$changement_commune <- renderDT({
    req(input$tabsetPanel == "Tableau des changments par commune")
    req(input$temps_select_tableau) # Permet de relancer lors d'un changement de temps
    # Exécuter la mise à jour du chemin de recherche uniquement après le changement de département et de période
    isolate({
      update_search_path(conn, input$depart_select_tableau, temps_vec_tableau[1], temps_vec_tableau[2])
    })
    
    chgt_com <- dbGetQuery(conn, "SELECT * FROM chgt_commune;")
    
    datatable(chgt_com, options = list(pageLength = 15, autoWidth = TRUE, ordering = TRUE), rownames = FALSE)
  })
}

# defusion fusion
shinyApp(ui, server)
