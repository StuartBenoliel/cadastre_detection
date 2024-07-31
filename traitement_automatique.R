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
#  85:95, 971:974, 976:978 
params_list <- list(
  list(num_departement = "89", temps_apres = 24, temps_avant = 23),
  list(num_departement = "88", temps_apres = 24, temps_avant = 23),
  list(num_departement = "87", temps_apres = 24, temps_avant = 23),
  list(num_departement = "86", temps_apres = 24, temps_avant = 23),
  list(num_departement = "5", temps_apres = 24, temps_avant = 23),
  list(num_departement = "4", temps_apres = 24, temps_avant = 23),
  list(num_departement = "3", temps_apres = 24, temps_avant = 23),
  list(num_departement = "2", temps_apres = 24, temps_avant = 23),
  list(num_departement = "1", temps_apres = 24, temps_avant = 23)
)
#Probleme 1 

params_list <- list(
  list(num_departement = "978", temps_apres = 24, temps_avant = 23),
  list(num_departement = "977", temps_apres = 24, temps_avant = 23),
  list(num_departement = "976", temps_apres = 24, temps_avant = 23),
  list(num_departement = "974", temps_apres = 24, temps_avant = 23),
  list(num_departement = "973", temps_apres = 24, temps_avant = 23),
  list(num_departement = "972", temps_apres = 24, temps_avant = 23),
  list(num_departement = "971", temps_apres = 24, temps_avant = 23)
)

params_list <- list(
  list(num_departement = "95", temps_apres = 24, temps_avant = 23),
  list(num_departement = "94", temps_apres = 24, temps_avant = 23),
  list(num_departement = "93", temps_apres = 24, temps_avant = 23),
  list(num_departement = "92", temps_apres = 24, temps_avant = 23),
  list(num_departement = "91", temps_apres = 24, temps_avant = 23),
  list(num_departement = "90", temps_apres = 24, temps_avant = 23),
  list(num_departement = "95", temps_apres = 23, temps_avant = 22),
  list(num_departement = "94", temps_apres = 23, temps_avant = 22),
  list(num_departement = "93", temps_apres = 23, temps_avant = 22),
  list(num_departement = "92", temps_apres = 23, temps_avant = 22),
  list(num_departement = "91", temps_apres = 23, temps_avant = 22),
  list(num_departement = "90", temps_apres = 23, temps_avant = 22),
  list(num_departement = "95", temps_apres = 22, temps_avant = 21),
  list(num_departement = "94", temps_apres = 22, temps_avant = 21),
  list(num_departement = "93", temps_apres = 22, temps_avant = 21),
  list(num_departement = "92", temps_apres = 22, temps_avant = 21),
  list(num_departement = "91", temps_apres = 22, temps_avant = 21),
  list(num_departement = "90", temps_apres = 22, temps_avant = 21),
  list(num_departement = "95", temps_apres = 21, temps_avant = 20),
  list(num_departement = "94", temps_apres = 21, temps_avant = 20),
  list(num_departement = "93", temps_apres = 21, temps_avant = 20),
  list(num_departement = "92", temps_apres = 21, temps_avant = 20),
  list(num_departement = "91", temps_apres = 21, temps_avant = 20),
  list(num_departement = "90", temps_apres = 21, temps_avant = 20),
  list(num_departement = "95", temps_apres = 20, temps_avant = 19),
  list(num_departement = "94", temps_apres = 20, temps_avant = 19),
  list(num_departement = "93", temps_apres = 20, temps_avant = 19),
  list(num_departement = "92", temps_apres = 20, temps_avant = 19),
  list(num_departement = "91", temps_apres = 20, temps_avant = 19),
  list(num_departement = "90", temps_apres = 20, temps_avant = 19)
)

params_list <- list(
  list(num_departement = "93", temps_apres = 23, temps_avant = 22)
)
# Boucle pour exécuter les blocs de code avec différents paramètres
for (params in params_list) {
  traitement_parcelles(conn, params$num_departement, 
                       params$temps_apres, params$temps_avant)
  print(paste0("Traitement département ", params$num_departement, 
               " pour les années 20", params$temps_apres, " - 20", params$temps_avant, " terminé !"))
}
