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
source("fonctions/fonction_create_db.R")
conn <- connecter()

# 2- Ajout de la base de données ####

# 1:19, 2a, 2b, 21:95, 971:978 
num_departements <- c('60')
num_annee <- 23
indic_parc <- T

# 8 min 1 département parcelle
for (i in 1:length(num_departements)){
  
  commune <- telechargement_departement(gestion_num_departement(toupper(num_departements[i])), 
                                 paste0("20", num_annee), 
                                 indic_parc)
  
  if(indic_parc) {
    commune <- traitement_doublon_et_arrondissement(commune)
  }
  
  constru_table(commune, num_departements[i], num_annee, indic_parc)
}

parc_85_24 <- process_departement('085', "2024")
parc_85_23 <- process_departement('085', "2023")
com_85 <- process_departement('085', "2024", F)

parc_85_22 <- process_departement('085', "2022")
parc_85_21 <- process_departement('085', "2021")

parc_21_24 <- process_departement("021", "2024")
parc_21_23 <- process_departement("021", "2023")
com_21 <- process_departement("021", "2024", F)

parc_13_24 <- process_departement("013", "2024")
parc_13_23 <- process_departement("013", "2023")
com_13 <- process_departement("013", "2024", F)


# Appliquer la fonction à chaque département
# list_parc <-lapply(num_departements,num_annee, process_departement)

# c- Construction et Exécution des requêtes pour créer les données sur la base postgis ####

# Construction de la requête créant les tables ####
conn <- connecter()

## Vendée
constru_table(com_85, F)
constru_table(parc_85_24)
constru_table(parc_85_23)

constru_table(parc_85_22)
parc_85_21 <- parc_85_21 %>% 
  filter(!(IDU == "851190000A1037" & FEUILLE == "5"))
# Doublon de ligne bizarre
constru_table(parc_85_21)

## Cote d'or
constru_table(com_21, F)
constru_table(parc_21_24)
parc_21_23 <- parc_21_23 %>% 
  filter(!(IDU == "213200000B0081" & FEUILLE == "5"))
# Doublon de ligne bizarre
constru_table(parc_21_23)

## PACA
constru_table(com_13, F)
constru_table(parc_13_24)
constru_table(parc_13_23)

dbListTables(conn)
dbGetQuery(conn, "SELECT schema_name FROM information_schema.schemata;")

# Déconnexion finale
dbDisconnect(conn)
