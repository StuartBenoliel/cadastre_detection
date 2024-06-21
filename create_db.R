# Créer une base de données postgis avec la plateforme datalab-sspcloud 

library(DBI)
library(RPostgres)
library(dplyr)
library(dbplyr)
library(sf)
library(archive)

# 0- Créer un service postgres avec l'extension postgis sur le datalab ####
rm(list=ls())

# 1- Connexion à la base de données ####
source("connexion_db.R", encoding = "utf-8")
conn <- connecter()

DBI::dbListObjects(conn)

# 2- Ajout de la base de données ####

# En téléchargeant depuis le site internet

num_departements <- c("45")  

# Fonction pour gérer le format des numéros de département
gestion_num_departement <- function(num_depart) {
  if (nchar(num_depart) == 2) {
    return(paste0("0", num_depart))
  } else {
    return(num_depart)
  }
}

# Appliquer la fonction à chaque département
num_departements <- sapply(num_departements, gestion_num_departement)

# Fonction pour télécharger et traiter chaque département
process_departement <- function(num_depart, num_annees, indic_parc = T) {
  # Construire l'URL du fichier .7z pour le département spécifié
  if (num_annees == 2024) {
    url <- paste0("https://data.geopf.fr/telechargement/download/PARCELLAIRE-EXPRESS/PARCELLAIRE-EXPRESS_1-1__SHP_LAMB93_D",
                  num_depart,
                  "_2024-04-01/PARCELLAIRE-EXPRESS_1-1__SHP_LAMB93_D",
                  num_depart,
                  "_2024-04-01.7z")
  }
  
  if(num_annees == 2023) {
    url <- paste0("https://files.opendatarchives.fr/professionnels.ign.fr/parcellaire-express/PARCELLAIRE_EXPRESS_1-1__SHP_LAMB93_D",
                  num_depart,
                  "_2023-07-01.7z")
  }
  
  temp <- tempfile(fileext = ".7z")
  download.file(url, temp, mode = "wb")
  temp_dir <- tempdir()
  archive_extract(temp, dir = temp_dir)
  
  if (num_annees == 2024) {
    shapefile_dir <- file.path(temp_dir, 
                               paste0("PARCELLAIRE-EXPRESS_1-1__SHP_LAMB93_D", 
                                      num_depart, 
                                      "_2024-04-01/PARCELLAIRE-EXPRESS/1_DONNEES_LIVRAISON_2024-05-00031/PEPCI_1-1_SHP_LAMB93_D", 
                                      num_depart))
  }
  
  if(num_annees == 2023) {
    shapefile_dir <- file.path(temp_dir, 
                               paste0("PARCELLAIRE_EXPRESS_1-1__SHP_LAMB93_D", 
                                      num_depart, 
                                      "_2023-07-01/PARCELLAIRE_EXPRESS/1_DONNEES_LIVRAISON_2023-07-00202/PEPCI_1-1_SHP_LAMB93_D", 
                                      num_depart))
  }
  
  # print(list.files(shapefile_dir))
  
  shapefile_path <- file.path(shapefile_dir, 
                              ifelse(indic_parc,"PARCELLE.SHP", "COMMUNE.SHP"))
  
  if (file.exists(shapefile_path)) {
    parc <- st_read(shapefile_path) %>%
      mutate(geometry = st_cast(geometry, "MULTIPOLYGON"))
    # Attention systeme de projection dans DOM
    print(str(parc))
  } else {
    stop("Le fichier shapefile spécifié n'a pas été trouvé dans l'archive.")
  }
  unlink(temp)
  unlink(temp_dir)
  return(parc)
}

parc_45 <- process_departement(num_departements, "2024")
parc_45_23 <- process_departement(num_departements, "2023")
com_45 <- process_departement(num_departements, "2024", F)

# Appliquer la fonction à chaque département
# list_parc <-lapply(num_departements,num_annees, process_departement)

# c- Construction et Exécution des requêtes pour créer les données sur la base postgis ####

# Construction de la requête créant les tables ####

constru_table <- function(table_sf, indic_parc = T) {
  if(indic_parc) {
    types_vars <- purrr::map_chr(
      names(table_sf)[-c(1,10,11)],
      function(var){
        paste0(var, " VARCHAR(", max(nchar(table_sf[[var]])), "), ")
      }
    ) %>% 
      paste0(., collapse="")
    
    query <- paste0(
      'CREATE TABLE ',
      deparse(substitute(table_sf)),
      ' (IDU VARCHAR PRIMARY KEY,',
      types_vars,
      'CONTENANCE INT,',
      'geometry GEOMETRY(MULTIPOLYGON, 2154));',
      collapse =''
    )
    
  } else {
    types_vars <- purrr::map_chr(
      names(table_sf)[-c(3,4)],
      function(var){
        paste0(var, " VARCHAR(", max(nchar(table_sf[[var]])), "), ")
      }
    ) %>% 
      paste0(., collapse="")
    
    query <- paste0(
      'CREATE TABLE ',
      deparse(substitute(table_sf)),
      ' (CODE_INSEE VARCHAR PRIMARY KEY,',
      types_vars,
      'geometry GEOMETRY(MULTIPOLYGON, 2154));',
      collapse =''
    )
  }
  
  # Création de la table (structure vide) ####
  dbSendQuery(conn, 
              paste0('DROP TABLE IF EXISTS ',
                     deparse(substitute(table_sf)),
                     ';'))
  dbSendQuery(conn, query)
  
  # Remplissage ####
  sf::st_write(
    obj = table_sf %>% rename_with(tolower),
    dsn = conn,
    Id(table = deparse(substitute(table_sf))),
    append = TRUE
  )
  
  # test lecture
  parc_head <- sf::st_read(conn, query = paste0('SELECT * FROM ',
                                                deparse(substitute(table_sf)),
                                                ' LIMIT 10;'))
  str(parc_head)
  
}

constru_table(parc_45)
constru_table(parc_45_23)
constru_table(com_45, F)

dbListTables(conn)

# Déconnexion finale
dbDisconnect(conn)
