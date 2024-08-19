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
  list(num_departement = "13", temps_apres = 24, temps_avant = 23),
  list(num_departement = "14", temps_apres = 21, temps_avant = 20),
  list(num_departement = "14", temps_apres = 22, temps_avant = 21),
  list(num_departement = "14", temps_apres = 23, temps_avant = 22),
  list(num_departement = "21", temps_apres = 21, temps_avant = 20),
  list(num_departement = "21", temps_apres = 22, temps_avant = 21),
  list(num_departement = "21", temps_apres = 23, temps_avant = 22),
  list(num_departement = "21", temps_apres = 24, temps_avant = 23),
  list(num_departement = "23", temps_apres = 21, temps_avant = 20),
  list(num_departement = "23", temps_apres = 22, temps_avant = 21),
  list(num_departement = "23", temps_apres = 23, temps_avant = 22),
  list(num_departement = "23", temps_apres = 24, temps_avant = 23)
)

params_list <- list(
  list(num_departement = "35", temps_apres = 24, temps_avant = 23),
  list(num_departement = "38", temps_apres = 20, temps_avant = 19),
  list(num_departement = "38", temps_apres = 21, temps_avant = 20),
  list(num_departement = "38", temps_apres = 22, temps_avant = 21),
  list(num_departement = "38", temps_apres = 23, temps_avant = 22),
  list(num_departement = "54", temps_apres = 21, temps_avant = 20),
  list(num_departement = "54", temps_apres = 22, temps_avant = 21),
  list(num_departement = "54", temps_apres = 23, temps_avant = 22),
  list(num_departement = "54", temps_apres = 24, temps_avant = 23),
  list(num_departement = "60", temps_apres = 24, temps_avant = 23)
)

params_list <- list(
  list(num_departement = "62", temps_apres = 21, temps_avant = 20),
  list(num_departement = "62", temps_apres = 22, temps_avant = 21),
  list(num_departement = "62", temps_apres = 23, temps_avant = 22),
  list(num_departement = "62", temps_apres = 24, temps_avant = 23),
  list(num_departement = "76", temps_apres = 20, temps_avant = 19),
  list(num_departement = "76", temps_apres = 21, temps_avant = 20),
  list(num_departement = "76", temps_apres = 22, temps_avant = 21),
  list(num_departement = "76", temps_apres = 23, temps_avant = 22),
  list(num_departement = "78", temps_apres = 23, temps_avant = 22),
  list(num_departement = "85", temps_apres = 24, temps_avant = 23)
)



# Boucle pour exécuter les blocs de code avec différents paramètres
for (params in params_list) {
  traitement_parcelles(conn, params$num_departement, 
                       params$temps_apres, params$temps_avant)
  print(paste0("Traitement département ", params$num_departement, 
               " pour les années 20", params$temps_apres, " - 20", params$temps_avant, " terminé !"))
}
