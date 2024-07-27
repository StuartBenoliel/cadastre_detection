library(shiny)
library(shinythemes)
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
source(file = "fonctions/fonction_shiny.R")
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
  br(),
  theme = shinytheme('flatly'),
  
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
                                  choices = NULL)
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
               column(6,
                      checkboxGroupInput("show_vars", "Variables à afficher:",
                                         NULL, selected = NULL, inline = T)
               ),
             ),
             wellPanel(class = "well-panel",
                       DTOutput("table")),
             br(),
             h3("Fusion / défusion de communes:"),
             wellPanel(class = "well-pane-small",
                       uiOutput("changement_communes"))
    ),
  ),
  
  tags$style(HTML("
    .well-panel {
      background-color: #fff;
      border-color: #2c3e50;
      height: 720px; /* Ensure the height is set */
      overflow-y: auto; /* Add vertical scrolling if content overflows */
      margin-bottom: 15px; /* Space between panels */
    }
    .well-panel-small {
      background-color: #fff;
      border-color: #2c3e50;
      min-height: 100px; /* Minimum height for small panels */
      max-height: 300px; /* Maximum height for small panels */
      overflow-y: auto;
      margin-bottom: 15px;
    }
    .dataTables_wrapper {
      overflow-x: auto;
    }
    .dataTables_scroll {
      overflow: hidden;
    }
    .dataTables_scrollBody {
      overflow-y: auto;
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
  
  rv <- reactiveVal(F)
  
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
    
    int_temps <- intervalle_temps(conn, num_departement)
    
    updateSelectInput(session, "temps_select_carte",
                      choices = int_temps,
                      selected = int_temps[1])
    update_search_path(conn, num_departement, temps_vec_carte[1], temps_vec_carte[2])
    
    commune <- load_communes(conn, num_departement, temps_vec_carte[1])
    
    commune <- sort(paste(commune$nom_com, commune$code_com))
    
    updateSelectInput(session, "nom_com_select_carte",
                      choices = commune,
                      selected = commune[1])
    
  })

  temps_reactive <- reactive({
    print(paste0("Changement au niveau de la période de temps carte: ", input$temps_select_carte))
    temps_vec_carte <<- strsplit(input$temps_select_carte, "-")[[1]]
    update_search_path(conn, num_departement, temps_vec_carte[1], temps_vec_carte[2])
    commune <- load_communes(conn, num_departement, temps_vec_carte[1])
    
    commune <- sort(paste(commune$nom_com, commune$code_com))
    
    updateSelectInput(session, "nom_com_select_carte",
                      choices = commune,
                      selected = commune[1])
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
    nom_com <-sub(" \\d+$", "",  gsub("'", "''", input$nom_com_select_carte))
    
    cartes_dynamiques(conn, input$depart_select_carte, temps_vec_carte[1], temps_vec_carte[2], nom_com)
  })
  
  # Lancement au démarage
  observeEvent(input$depart_select_tableau, {
    print(paste0("Changement au niveau du département tableau: ", input$depart_select_tableau))
    int_temps <- intervalle_temps(conn, input$depart_select_tableau)
    temps_vec_tableau <<- strsplit(int_temps[1], "-")[[1]]

    updateSelectInput(session, "temps_select_tableau",
                      choices = int_temps,
                      selected = int_temps[1])
    
    vars <- c('nom_com', 'code_com', 
                 paste0('parcelles_20', temps_vec_tableau[1]), 
                 paste0('parcelles_20', temps_vec_tableau[2]), 
                 paste0('restantes_20', temps_vec_tableau[1]),
                 paste0('restantes_20', temps_vec_tableau[2]), 
                 'vrai_ajout', 'vrai_supp', 
                 'translation', 'contour', 
                 'contour_translation', 'subdiv', 
                 'fusion', 'redecoupage', 
                 'contour_transfo','contour_transfo_translation')
    
    updateCheckboxGroupInput(session, "show_vars",
                             choices = vars,
                             selected = vars, inline = T)
    
  })
  
  observeEvent(input$temps_select_tableau, {
    req(input$tabsetPanel == "Tableau des changments par commune")
    print(paste0("Changement au niveau de la période de temps tableau: ", input$temps_select_tableau))
    temps_vec_tableau <<- strsplit(input$temps_select_tableau, "-")[[1]]
    
    var <- c('nom_com', 'code_com', paste0('parcelles_20',temps_vec_tableau[1]), 
             paste0('parcelles_20',temps_vec_tableau[2]), paste0('restantes_20',temps_vec_tableau[1]),
             paste0('restantes_20',temps_vec_tableau[2]), 'vrai_ajout', 'vrai_supp', 
             'translation', 'contour', 'contour_translation', 'subdiv', 
             'fusion', 'redecoupage', 'contour_transfo','contour_transfo_translation')
    
    updateCheckboxGroupInput(session, "show_vars",
                             choices = var,
                             selected = var, inline = T)
  })
  
  output$table <- renderDT({
    print(rv())
    req(input$tabsetPanel == "Tableau des changments par commune")
    req(input$temps_select_tableau) # Permet de relancer lors d'un changement de temps
    # Exécuter la mise à jour du chemin de recherche uniquement après le changement de département et de période
    isolate({
      update_search_path(conn, input$depart_select_tableau, temps_vec_tableau[1], temps_vec_tableau[2])
    })
    print(paste0("Affichage du tableau des changements au niveau du département: ", input$depart_select_tableau))
    tableau_recap(conn, input$depart_select_tableau, 
                  temps_vec_tableau[1], temps_vec_tableau[2], input$show_vars)
  })
  
  output$changement_communes <- renderUI({
    req(input$tabsetPanel == "Tableau des changments par commune")
    req(input$temps_select_tableau)

    print(paste0("Affichage du tableau des fusion/défusions au niveau du département: ", input$depart_select_tableau))
    
    isolate({
      update_search_path(conn, input$depart_select_tableau, temps_vec_tableau[1], temps_vec_tableau[2])
    })
    
    chgt_com <- dbGetQuery(conn, "SELECT * FROM chgt_commune;")
    
    tagList(
      # Render the small data table if it has content
      if (nrow(chgt_com) > 0) {
        datatable(chgt_com, options = list(paging = FALSE, searching = FALSE, 
                                           autoWidth = TRUE, ordering = TRUE), rownames = FALSE)
      } else {
        # Alternative content if the data is empty or not available
        h4("Aucune")
      }
    )
  })
}

# defusion fusion
shinyApp(ui, server)
