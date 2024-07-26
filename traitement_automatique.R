library(sf)
library(dplyr)
library(DBI)
library(stringr)

rm(list = ls())

source(file = "database/connexion_db.R")
conn <- connecter()
source(file = "fonctions/fonction_traitement_parcelles.R")

# Liste des paramètres à utiliser
params_list <- list(
  list(num_departement = "85", temps_apres = 24, temps_avant = 23),
  list(num_departement = "85", temps_apres = 23, temps_avant = 22),
  list(num_departement = "60", temps_apres = 24, temps_avant = 23),
  list(num_departement = "2b", temps_apres = 24, temps_avant = 23),
  list(num_departement = "2a", temps_apres = 24, temps_avant = 23),
  list(num_departement = "21", temps_apres = 24, temps_avant = 23),
  list(num_departement = "13", temps_apres = 24, temps_avant = 23)
)
indic_bordure <- F

# Boucle pour exécuter les blocs de code avec différents paramètres
for (params in params_list) {
  traitement_parcelles(conn, params$num_departement, 
                       params$temps_apres, params$temps_avant)
  print(paste0("Traitement déparement ", params$num_departement, 
               " pour les années 20", params$temps_apres, " - 20", params$temps_avant, " terminé !"))
}
