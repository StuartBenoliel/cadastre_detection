# Créer une base de données postgis avec la plateforme datalab-sspcloud 

library(DBI)
library(RPostgres)
library(dplyr)
library(dbplyr)
library(sf)
library(archive)

# 0- Créer un service postgres avec l'extension postgis sur le datalab ####
rm(list=ls())
options(timeout = 600)

# 1- Connexion à la base de données ####
source("connexion_db.R", encoding = "utf-8")
conn <- connecter()

DBI::dbListObjects(conn)

# 2- Ajout de la base de données ####

# En téléchargeant depuis le site internet

num_departements <- c("85")  

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
  
  archive_months <- c(
    "2022" = "01",
    "2021" = "02",
    "2020" = "01",
    "2019" = "07"
  )
  
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
  
  if (num_annees %in% names(archive_months)) {
    url <- paste0("https://files.opendatarchives.fr/professionnels.ign.fr/parcellaire-express/PCI-par-DEPT_",
                  num_annees,
                  "-",
                  archive_months[num_annees],
                  "/PARCELLAIRE_EXPRESS_1-0__SHP_LAMB93_D",
                  num_depart,
                  "_",
                  num_annees,
                  "-",
                  archive_months[num_annees],
                  "-01.7z")
  }
  
  temp <- tempfile(fileext = ".7z")
  download.file(url, temp, mode = "wb")
  temp_dir <- tempdir()
  archive_extract(temp, dir = temp_dir)
  
  fichier_parcelle <- list.files(temp_dir, 
                                 pattern = ifelse(indic_parc,"PARCELLE.SHP", "COMMUNE.SHP"), 
                                 recursive = TRUE, full.names = TRUE)
  
  # Vérifier si le fichier a été trouvé
  if(length(fichier_parcelle) > 0) {
    parc <- st_read(fichier_parcelle) %>%
      mutate(geometry = st_cast(geometry, "MULTIPOLYGON"))
    # Attention systeme de projection dans DOM
    print(str(parc))
    
  } else {
    print("Fichier non trouvé.")
  }
  unlink(temp)
  unlink(temp_dir)
  return(parc)
}

parc_85 <- process_departement(num_departements, "2024")
parc_85_23 <- process_departement(num_departements, "2023")
com_85 <- process_departement(num_departements, "2024", F)

parc_21 <- process_departement("021", "2022")
parc_21_23 <- process_departement("021", "2023")
com_21 <- process_departement("021", "2024", F)

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

parc_21_23 <- parc_21_23 %>% 
  filter(!(IDU == "213200000B0081" & FEUILLE == "5"))
# Doublon de ligne bizarre

constru_table(parc_85)
constru_table(parc_85_23)
constru_table(com_85, F)

constru_table(parc_21)
constru_table(parc_21_23)
constru_table(com_21, F)


dbListTables(conn)

# Déconnexion finale
dbDisconnect(conn)
