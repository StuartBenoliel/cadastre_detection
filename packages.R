# Liste des packages nécessaires
packages <- c(
  "shiny",
  "bslib",
  "DT",
  "archive",
  "DBI",
  "dplyr",
  "stringr",
  "sf",
  "mapview",
  "leaflet",
  "leafsync",
  "webshot",
  "ggplot2"
)

# Installation des packages
install.packages(setdiff(packages, rownames(installed.packages()))) 