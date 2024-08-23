# Créer une base de données postgis avec la plateforme datalab-sspcloud 

library(DBI)
library(dplyr)
library(sf)
library(archive)

# 0- Créer un service postgres avec l'extension postgis sur le datalab ####
rm(list=ls())
options(timeout = 6000)

# 1- Connexion à la base de données ####
source("database/connexion_db.R", encoding = "utf-8")
source("fonctions/fonction_creation_db.R")
conn <- connecter()

# 2- Ajout de la base de données ####
# 1:5, 13, 21, 2a, 2b, 60, 85:95, 971:974, 976:978 
# 1:19, 2a, 2b, 21:95, 971:974, 976:978
# 14, 21, 23, 38, 54, 62, 76, 85, 91

# 21 -> 20/21 /22 /23 /24
# 23 -> 23/24 D
# 38 -> 19/20 D
# 54 -> 20/21 /22/23/24 D 
#91 -> 20/21/22

params_list <- list(
  list(num_departement = "2b", num_annee = 23, indicatrice_parcelle = F),
  list(num_departement = "2b", num_annee = 22, indicatrice_parcelle = F)
)

# Boucle pour exécuter les blocs de code avec différents paramètres
for (params in params_list) {
  
  donnees <- telechargement_departement(gestion_num_departement(toupper(params$num_departement)), 
                                        paste0("20", params$num_annee), 
                                        params$indicatrice_parcelle)
  
  if(params$indicatrice_parcelle) {
    donnees <- traitement_doublon_et_arrondissement(donnees)
  }
  
  conn <- connecter() # Pour récupérer la connexion si le téléchargement prend beaucoup de temps
  
  constru_table(donnees, as.character(params$num_departement), params$num_annee, 
                params$indicatrice_parcelle)
  
  print(paste0("Import département ", params$num_departement, 
               " pour l'années 20", params$num_annee, " terminé !"))
}
# 8 min 1 département parcelle

# dbGetQuery(conn, "SELECT schema_name FROM information_schema.schemata;")

# Déconnexion finale
dbDisconnect(conn)
