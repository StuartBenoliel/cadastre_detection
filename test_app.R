library(shiny)
library(shiny.semantic)

ui <- semanticPage(
  title = "Exemple de cartes",
  
  fluidRow(
    # Première carte
    semanticCard(
      title = "Carte 1",
      content = "Contenu de la première carte.",
      image = "https://via.placeholder.com/150"
    ),
    # Deuxième carte
    semanticCard(
      title = "Carte 2",
      content = "Contenu de la deuxième carte.",
      image = "https://via.placeholder.com/150"
    )
  )
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)
