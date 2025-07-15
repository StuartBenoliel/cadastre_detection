# Liste des packages nécessaires
packages <- c(
  "shiny",
  "bslib",
  "bsicons",
  "DT",
  "RPostgres",
  "archive",
  "DBI",
  "dplyr",
  "stringr",
  "sf",
  "mapview",
  "leaflet",
  "leafsync",
  "leaflet.extras2",
  "webshot",
  "ggplot2"
)

# Installation des packages
install.packages(setdiff(packages, rownames(installed.packages()))) 
