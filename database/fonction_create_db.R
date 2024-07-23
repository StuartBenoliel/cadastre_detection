library(DBI)
library(RPostgres)
library(dplyr)
library(dbplyr)
library(sf)
library(archive)

# Fonction pour gérer le format des numéros de département
gestion_num_departement <- function(num_depart) {
  while (nchar(num_depart) < 3) {
    num_depart <- paste0("0", num_depart)
  }
  return(num_depart)
}

# Fonction pour télécharger et traiter chaque département
process_departement <- function(num_depart, num_annee, indic_parc = T) {
  
  archive_months <- c(
    "2022" = "01",
    "2021" = "02",
    "2020" = "01",
    "2019" = "07"
  )
  
  # Construire l'URL du fichier .7z pour le département spécifié
  if (num_annee == 2024) {
    url <- paste0("https://data.geopf.fr/telechargement/download/PARCELLAIRE-EXPRESS/PARCELLAIRE-EXPRESS_1-1__SHP_LAMB93_D",
                  num_depart,
                  "_2024-04-01/PARCELLAIRE-EXPRESS_1-1__SHP_LAMB93_D",
                  num_depart,
                  "_2024-04-01.7z")
  }
  
  if(num_annee == 2023) {
    url <- paste0("https://files.opendatarchives.fr/professionnels.ign.fr/parcellaire-express/PARCELLAIRE_EXPRESS_1-1__SHP_LAMB93_D",
                  num_depart,
                  "_2023-07-01.7z")
  }
  
  if (num_annee %in% names(archive_months)) {
    url <- paste0("https://files.opendatarchives.fr/professionnels.ign.fr/parcellaire-express/PCI-par-DEPT_",
                  num_annee,
                  "-",
                  archive_months[num_annee],
                  "/PARCELLAIRE_EXPRESS_1-0__SHP_LAMB93_D",
                  num_depart,
                  "_",
                  num_annee,
                  "-",
                  archive_months[num_annee],
                  "-01.7z")
  }
  
  temp <- tempfile(fileext = ".7z")
  download.file(url, temp, mode = "wb")
  temp_dir <- tempdir()
  archive_extract(temp, dir = temp_dir)
  unlink(temp)
  # print(list.files(temp_dir))
  fichier_parcelle <- list.files(temp_dir, 
                                 pattern = ifelse(indic_parc,"PARCELLE.SHP", "COMMUNE.SHP"), 
                                 recursive = TRUE, full.names = TRUE)
  
  if(length(fichier_parcelle) > 0) {
    parc <- st_read(fichier_parcelle) %>%
      st_make_valid() %>% # st_buffer st_valid_reason where st-valid false
      mutate(geometry = st_cast(geometry, "MULTIPOLYGON"))
    # Attention systeme de projection dans DOM
    print(str(parc))
    
  } else {
    print("Fichier non trouvé.")
  }
  
  if(indic_parc) {
    parc <- parc %>% 
      mutate(
        NOM_COM = case_when(
          endsWith(CODE_ARR, "00") ~ NOM_COM,
          endsWith(CODE_ARR, "01") ~ paste0(NOM_COM, " 1er"),
          TRUE ~ paste0(
            NOM_COM, " ",
            case_when(
              endsWith(substring(CODE_ARR, 2, 2), "0") ~ paste0(substring(CODE_ARR, 3, 3), "e"),
              TRUE ~ paste0(sub("^0", "", substring(CODE_ARR, 2, 3)), "e")
            ), "me"
          )),
        CONTENANCE = round(st_area(geometry), 1)) %>% select(IDU, NOM_COM, CODE_COM, COM_ABS, CONTENANCE)
  }
  files <- list.files(temp_dir, full.names = TRUE)
  # Filtrer les fichiers commencant par "PARCELLAIRE"
  parcellaire_file <- files[grep("^PARCELLAIRE", basename(files))]
  unlink(parcellaire_file, recursive = TRUE)
  return(parc)
}

constru_table <- function(table_sf, num_departement, num_annee, indic_parc = T) {
  
  dbExecute(conn, paste0("CREATE SCHEMA IF NOT EXISTS cadastre_", num_departement))
  dbExecute(conn, paste0("SET search_path TO cadastre_", num_departement, ", public"))
  
  if(indic_parc) {
    types_vars <- purrr::map_chr(
      names(table_sf)[-c(1,5,6)],
      function(var){
        paste0(var, " VARCHAR(", max(nchar(table_sf[[var]])), "), ")
      }
    ) %>% 
      paste0(., collapse="")
    
    # Création de la table (structure vide) ####
    
    dbExecute(conn, paste0(
      'DROP TABLE IF EXISTS parc_', num_departement, '_', num_annee, ';'))
    
    dbExecute(conn, paste0(
      'CREATE TABLE parc_', num_departement, '_', num_annee,
      ' (IDU VARCHAR(14) PRIMARY KEY,',
      types_vars,
      'CONTENANCE NUMERIC,',
      'geometry GEOMETRY(MULTIPOLYGON, 2154));'
    ))
    
    dbExecute(conn,paste0(
      'CREATE INDEX ',
      'idx_parc_', num_departement, '_', num_annee, '_geometry',
      ' ON parc_', num_departement, '_', num_annee,
      ' USING GIST(geometry);'))
    
    # Remplissage ####
    sf::st_write(
      obj = table_sf %>% rename_with(tolower),
      dsn = conn,
      Id(table =  paste0('parc_', num_departement, '_', num_annee)),
      append = TRUE
    )
    
  } else {
    types_vars <- purrr::map_chr(
      names(table_sf)[-c(2,3,4)],
      function(var){
        paste0(var, " VARCHAR(", max(nchar(table_sf[[var]])), "), ")
      }
    ) %>% 
      paste0(., collapse="")
    
    # Création de la table (structure vide) ####
    
    dbExecute(conn, paste0('DROP TABLE IF EXISTS com_', num_departement, ';'))
    
    dbExecute(conn, paste0(
      'CREATE TABLE com_', num_departement,
      ' (CODE_INSEE VARCHAR(5) PRIMARY KEY,',
      types_vars,
      'geometry GEOMETRY(MULTIPOLYGON, 2154));',
      collapse =''
    ))
    
    dbExecute(conn,paste0(
      'CREATE INDEX ',
      'idx_com_', num_departement, '_geometry',
      ' ON com_', num_departement,
      ' USING GIST(geometry);'))
    
    # Remplissage ####
    sf::st_write(
      obj = table_sf %>% select(NOM_COM, CODE_INSEE) %>%  rename_with(tolower),
      dsn = conn,
      Id(table =  paste0('com_', num_departement)),
      append = TRUE
    )
  }
}