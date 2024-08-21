library(shiny)
library(bslib)
library(sf)
library(mapview)
library(leaflet)
library(leaflet.extras2)
library(stringr)
library(leafsync)
library(DBI)
library(DT)

rm(list=ls())
source(file = "database/connexion_db.R")
source(file = "fonctions/fonction_shiny.R")

conn <- connecter()
departements <- departement_traite(conn)
style <- HTML("
    .input-group {
      display: flex;
      align-items: bottom;
    }
    .info-icon {
      color: black;
      font-size: 18px;
      cursor: pointer;
      margin-left: 30px;
    }
    .info-icon:hover {
      color: #0056b3;
    }
    .icon-large {
      font-size: 40px; /* Ajustez la taille de l'icône selon vos besoins */
    }
    .well-panel {
      background-color: #fff;
      border-color: #2c3e50;
      height: 840px; /* Ensure the height is set */
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
    }")


ui <- page_navbar(
  id = "main_navbar",
  title = "Cadastat : détection des évolutions des parcelles cadastrales",
  nav_spacer(), # push nav items to the right
  nav_panel("Carte des comparaisons par commune",
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
              column(6,
                     div(class = "input-group",
                         selectizeInput("nom_com_select_carte", 
                                        label = "Choisir une / plusieurs communes:",
                                        choices = NULL,
                                        multiple = TRUE,
                                        options = list(plugins = "remove_button"),
                                        width = "60%"),
                         # Ajouter un icône avec une info-bulle à droite du selectInput
                         tags$span(
                           title = "Les communes proposées sont les communes existantes à l'année la plus tardive de la période de temps choisie.",
                           class = "info-icon",
                           icon("info-circle"),
                           style = "margin-left: 10px; margin-bottom: 60px; display: flex; align-items: center;"
                         )
                     )
              ),
            ),
            wellPanel(class = "well-panel", uiOutput("dynamicMaps")),
            h3("Cas de parcelles avec géomètrie absente:"),
            wellPanel(class = "well-pane-small", uiOutput("parcelles_absentes")),
            br()
            
  ),
  nav_panel("Tableau des changments par commune",
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
              column(3,
                     div(style = "margin-left: 10px; margin-top: 15px; display: flex; align-items: center;",
                         span(style = "margin-right: 10px;","Filtrage des colonnes:"),
                         popover(
                           bsicons::bs_icon("gear", class = "icon-large", title = "Settings"),
                           checkboxGroupInput("var_tableau", "Variables à afficher:",
                                              NULL, selected = NULL, inline = TRUE)
                         )
                     )
              ),
            ),
            wellPanel(class = "well-panel", DTOutput("table")),
            h3("Fusion / scission de communes / changement de nom:"),
            wellPanel(class = "well-pane-small", uiOutput("changement_communes")),
            br()
  ),
  nav_item(img(src = "insee-logo.jpg", height = "50px", align = "right")),
  header = tags$style(style)
)

server <- function(input, output, session) {
  
  indic <- reactiveVal(FALSE)
  indic_double <- reactiveVal(FALSE)
  
  observe({
    print("Reconnexion à la base de données")
    keep_alive(conn)
  })
  
  # Lancement au démarage
  observeEvent(input$depart_select_carte, {
    print(paste0("Changement au niveau du département carte: ", input$depart_select_carte))
    
    int_temps <<- intervalle_temps(conn, input$depart_select_carte)
    
    if (input$temps_select_carte %in% int_temps) {
      temps_select <- input$temps_select_carte
    } else {
      temps_select <- int_temps[1]
    }
    updateSelectInput(session, "temps_select_carte",
                      choices = int_temps, 
                      selected = temps_select)
    
    temps_vec <<- strsplit(temps_select, "-")[[1]]
    
    if (input$temps_select_carte != temps_select){
      indic(FALSE) 
    } else{
      indic(TRUE)
      if (indic_double()){
        maj_chemin(conn, input$depart_select_carte, temps_vec[1], temps_vec[2])
        commune <<- nom_code_commune(conn, input$depart_select_carte, temps_vec[1])
        updateSelectizeInput(session, "nom_com_select_carte",
                             choices = commune, 
                             selected = commune[1])
      }
    }
  })
  
  observeEvent(input$temps_select_carte, {
    req(input$temps_select_carte) # Evite que cela se lance avant la 1ere initialisation
    print(paste0("Changement au niveau de la période de temps carte: ", input$temps_select_carte))
    
    temps_vec <<- strsplit(input$temps_select_carte, "-")[[1]]
    maj_chemin(conn, input$depart_select_carte, temps_vec[1], temps_vec[2])
    
    commune <<- nom_code_commune(conn, input$depart_select_carte, temps_vec[1])
    updateSelectizeInput(session, "nom_com_select_carte",
                         choices = commune, 
                         selected =  {
                           if (!is.null(input$nom_com_select_carte) && all(input$nom_com_select_carte %in% commune)) {
                             input$nom_com_select_carte
                           } else {
                             commune[1]
                           }
                         })
    indic(TRUE)
    indic_double(TRUE) 
  })
  
  # Rendu dynamique des cartes
  output$dynamicMaps <- renderUI({
    req(input$main_navbar == "Carte des comparaisons par commune")
    req(input$temps_select_carte) # Permet de relancer lors d'un changement de temps
    req(indic())
    
    if (all(input$nom_com_select_carte %in% commune) & !is.null(input$nom_com_select_carte)){
      nom_com <- sub(" \\d+$", "",  gsub("'", "''", input$nom_com_select_carte))
      nom_com <- paste0("'", paste(nom_com, collapse = "', '"), "'")
      print(paste0("Affichage des cartes pour la commune: ", nom_com))
      
      indic_refonte <- check_refonte_pc(conn, nom_com)
      if (indic_refonte) {
        showNotification("Refonte du plan cadastral de la commune probable. Classification non fiable.",
                         type = "warning", duration = NULL)
      }
      cartes_dynamiques(conn, input$depart_select_carte, temps_vec[1], temps_vec[2], nom_com)
    } 
  })
  
  output$parcelles_absentes <- renderUI({
    req(input$main_navbar == "Carte des comparaisons par commune")
    req(input$temps_select_carte) # Permet de relancer lors d'un changement de temps
    req(indic())
    
    if (all(input$nom_com_select_carte %in% commune) & !is.null(input$nom_com_select_carte)){
      nom_com <- sub(" \\d+$", "",  gsub("'", "''", input$nom_com_select_carte))
      nom_com <- paste0("'", paste(nom_com, collapse = "', '"), "'")
      
      parc_null <- dbGetQuery(conn, paste0(
        "SELECT idu, nom_com, code_com, com_abs, '20", temps_vec[1], "' AS période 
            FROM parc_", input$depart_select_carte, "_", temps_vec[1], " 
            WHERE ST_IsEmpty(geometry) AND  nom_com IN (", nom_com, ")
        UNION ALL
        SELECT idu, nom_com, code_com, com_abs, '20", temps_vec[2], "' AS période 
            FROM parc_", input$depart_select_carte, "_", temps_vec[2], " 
            WHERE ST_IsEmpty(geometry) AND  nom_com IN (", nom_com, ");"
      ))
      tableau_si_donnee(parc_null)
    } 
  })
  
  
  observeEvent(input$depart_select_tableau, {
    req(input$main_navbar == "Tableau des changments par commune") # Evite que cela se lance avant la 1ere initialisation
    print(paste0("Changement au niveau du département tableau: ", input$depart_select_tableau))
    int_temps <<- intervalle_temps(conn, input$depart_select_tableau)
    
    if (input$temps_select_tableau %in% int_temps) {
      temps_select <- input$temps_select_tableau
    } else {
      temps_select <- int_temps[1]
    }
    
    updateSelectInput(session, "temps_select_tableau",
                      choices = int_temps,
                      selected = temps_select)
    
    temps_vec <<- strsplit(temps_select, "-")[[1]]
    
    if (input$temps_select_tableau != temps_select){
      indic(FALSE) 
    } else{
      indic(TRUE)
      if (indic_double()){
        maj_chemin(conn, input$depart_select_tableau, temps_vec[1], temps_vec[2])
      }
    }
    
  })
  
  observeEvent(input$temps_select_tableau, {
    req(input$main_navbar == "Tableau des changments par commune") # Evite que cela se lance avant la 1ere initialisation
    print(paste0("Changement au niveau de la période de temps tableau: ", input$temps_select_tableau))
    
    temps_vec <<- strsplit(input$temps_select_tableau, "-")[[1]]
    col_tableau <- c('nom', 'code', paste0('total_20',temps_vec[1]), 
                     paste0('total_20',temps_vec[2]), 
                     paste0('taux_classif_20',temps_vec[1]),
                     paste0('taux_classif_20',temps_vec[2]), 
                     'ajout', 'suppression', 'translation', 'contour', 
                     'redécoupage', 'contour_redécoupage', 'échange')
    
    updateCheckboxGroupInput(session, "var_tableau",
                             choices = col_tableau,
                             selected = col_tableau, inline = T)
    
    maj_chemin(conn, input$depart_select_tableau, temps_vec[1], temps_vec[2])
    
    indic(FALSE) 
    indic_double(TRUE) 
  })
  
  observeEvent(input$var_tableau, {
    indic(TRUE)
  })
  
  output$table <- renderDT({
    req(input$main_navbar == "Tableau des changments par commune")
    req(indic())
    print(paste0("Affichage du tableau des changements au niveau du département: ", input$depart_select_tableau))
    
    tableau_recap(conn, input$depart_select_tableau, 
                  temps_vec[1], temps_vec[2], input$var_tableau)
    
  })
  
  output$changement_communes <- renderUI({
    req(input$main_navbar == "Tableau des changments par commune")
    req(indic())
    req(input$depart_select_tableau)
    
    chgt_com <- dbGetQuery(conn, "SELECT * FROM chgt_com;")
    tableau_si_donnee(chgt_com)
  })
  
  observeEvent(input$main_navbar, {
    req(input$temps_select_carte) # Evite que cela se lance au démarage
    print("Changement d'onglet")
    if (input$main_navbar == "Carte des comparaisons par commune") {
      
      ifelse(input$depart_select_carte == input$depart_select_tableau & 
               input$temps_select_carte == input$temps_select_tableau, indic(TRUE), indic(FALSE))
      ifelse(input$depart_select_carte != input$depart_select_tableau & 
               input$temps_select_carte != input$temps_select_tableau, indic_double(FALSE), indic_double(TRUE))
      
      updateSelectInput(session, "depart_select_carte", selected = input$depart_select_tableau)
      updateSelectInput(session, "temps_select_carte",
                        choices = int_temps, 
                        selected = ifelse(input$temps_select_tableau %in% int_temps, 
                                          input$temps_select_tableau, int_temps[1]))
    } else {
      
      ifelse(input$depart_select_carte == input$depart_select_tableau & 
               input$temps_select_carte == input$temps_select_tableau, indic(TRUE) , indic(FALSE))
      ifelse(input$depart_select_carte != input$depart_select_tableau & 
               input$temps_select_carte != input$temps_select_tableau, indic_double(FALSE), indic_double(TRUE))
      
      updateSelectInput(session, "depart_select_tableau", selected = input$depart_select_carte)
      updateSelectInput(session, "temps_select_tableau",
                        choices = int_temps,
                        selected = ifelse(input$temps_select_carte %in% int_temps, 
                                          input$temps_select_carte, int_temps[1]))
    }
  })
}


shinyApp(ui, server)
