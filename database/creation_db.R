# Créer une base de données postgis avec la plateforme datalab-sspcloud 

library(DBI)
library(RPostgres)
library(dplyr)
library(dbplyr)
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

num_departements <- c(35)
num_annee <- 23
indic_parc <- T
# 21 -> 20/21 /22 /23 /24
# 23 -> 23/24 D
# 38 -> 19/20 D
# 54 -> 20/21 /22/23/24 D 
#91 -> 20/21/22

# 8 min 1 département parcelle
for (i in 1:length(num_departements)){
  
  commune <- telechargement_departement(gestion_num_departement(toupper(num_departements[i])), 
                                 paste0("20", num_annee), 
                                 indic_parc)
  
  if(indic_parc) {
    commune <- traitement_doublon_et_arrondissement(commune)
  }
  conn <- connecter()
  constru_table(commune, as.character(num_departements[i]), num_annee, indic_parc)
  
  print(paste0("Import département ", num_departements[i], 
               " pour l'années 20", num_annee, " terminé !"))
}

parc_85_21 <- parc_85_21 %>% 
  filter(!(IDU == "851190000A1037" & FEUILLE == "5"))
# Doublon de ligne bizarre

parc_21_23 <- parc_21_23 %>% 
  filter(!(IDU == "213200000B0081" & FEUILLE == "5"))
# Doublon de ligne bizarre

dbListTables(conn)
dbGetQuery(conn, "SELECT schema_name FROM information_schema.schemata;")

# Déconnexion finale
dbDisconnect(conn)
