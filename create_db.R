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

num_departements <- c("45", "22", "972")  

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
process_departement <- function(num_depart) {
  # Construire l'URL du fichier .7z pour le département spécifié
  url <- paste0("https://data.geopf.fr/telechargement/download/PARCELLAIRE-EXPRESS/PARCELLAIRE-EXPRESS_1-1__SHP_LAMB93_D",
                num_depart,
                "_2024-04-01/PARCELLAIRE-EXPRESS_1-1__SHP_LAMB93_D",
                num_depart,
                "_2024-04-01.7z")
  
  # Télécharger le fichier .7z
  temp <- tempfile(fileext = ".7z")
  download.file(url, temp, mode = "wb")
  temp_dir <- tempdir()
  archive_extract(temp, dir = temp_dir)
  
  shapefile_dir <- file.path(temp_dir, 
                             paste0("PARCELLAIRE-EXPRESS_1-1__SHP_LAMB93_D", num_depart, "_2024-04-01/PARCELLAIRE-EXPRESS/1_DONNEES_LIVRAISON_2024-05-00031/PEPCI_1-1_SHP_LAMB93_D", num_depart))
  
  shapefile_path <- file.path(shapefile_dir, "PARCELLE.SHP")
  if (file.exists(shapefile_path)) {
    parc <- st_read(shapefile_path)
    print(str(parc))
  } else {
    stop("Le fichier shapefile spécifié n'a pas été trouvé dans l'archive.")
  }
  return(parc)
  # Supprimer les fichiers temporaires
  unlink(temp)
  unlink(temp_dir, recursive = TRUE)
}

# Appliquer la fonction à chaque département
list_parc <- sapply(num_departements, process_departement)

unlink(temp)
unlink(temp_dir, recursive = TRUE)

# Si importé manuellement
parc_45<-st_read("./parcelle-45-IGN/PARCELLE.SHP")


parc_45 <- parc_45 %>%
  mutate(geometry = st_cast(geometry, "MULTIPOLYGON"))
# Attention systeme de projection dans DOM

# c- Construction et Exécution des requêtes pour créer les données sur la base postgis ####

# Construction de la requête créant les tables ####
types_vars_parc <- purrr::map_chr(
  names(parc_45)[-c(1,10,11)],
  function(var){
    paste0(var, " VARCHAR(", max(nchar(parc_45[[var]])), "), ")
  }
) %>% 
  paste0(., collapse="")

query <- paste0(
  'CREATE TABLE parc_45',
  ' (IDU VARCHAR PRIMARY KEY,',
  types_vars_parc,
  'CONTENANCE INT,',
  'geometry GEOMETRY(MULTIPOLYGON, 2154));',
  collapse =''
)

# Création de la table (structure vide) ####
dbSendQuery(conn, 'DROP TABLE IF EXISTS parc_45;')
dbSendQuery(conn, query)

types_vars_parc <- purrr::map_chr(
  names(parc_45_23)[-c(1,10,11)],
  function(var){
    paste0(var, " VARCHAR(", max(nchar(parc_45_23[[var]])), "), ")
  }
) %>% 
  paste0(., collapse="")

query <- paste0(
  'CREATE TABLE parc_45_23',
  ' (IDU VARCHAR PRIMARY KEY,',
  types_vars_parc,
  'CONTENANCE INT,',
  'geometry GEOMETRY(MULTIPOLYGON, 2154));',
  collapse =''
)

# Création de la table (structure vide) ####
dbSendQuery(conn, 'DROP TABLE IF EXISTS parc_45_23;')
dbSendQuery(conn, query)

dbListTables(conn)

# Remplissage ####
sf::st_write(
  obj = parc_45 %>% rename_with(tolower),
  dsn = conn,
  Id(table = 'parc_45'),
  append = TRUE
)

sf::st_write(
  obj = parc_45_23 %>% rename_with(tolower),
  dsn = conn,
  Id(table = 'parc_45_23'),
  append = TRUE
)

# test lecture
parc_head <- sf::st_read(conn, query = 'SELECT * FROM parc_45 LIMIT 10;')
str(parc_head)
st_crs(parc_head)

# Déconnexion finale
dbDisconnect(conn)
