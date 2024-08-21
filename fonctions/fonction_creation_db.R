# Fonction pour gérer le format des numéros de département
gestion_num_departement <- function(num_departement) {
  while (nchar(num_departement) < 3) {
    num_departement <- paste0("0", num_departement)
  }
  return(num_departement)
}

sys_projection <- function(num_departement) {
  syst_proj <- c(
    "971" = "RGAF09UTM20",
    "972" = "RGAF09UTM20",
    "973" = "UTM22RGFG95",
    "974" = "RGR92UTM40S",
    "976" = "RGM04UTM38S",
    "977" = "RGAF09UTM20",
    "978" = "RGAF09UTM20"
  )
  if (num_departement %in% names(syst_proj)) {
    return(syst_proj[[num_departement]])
  } else {
    return("LAMB93")
  }
}

code_sys_projection <- function(num_departement) {
  syst_proj <- c(
    "971" = 5490,
    "972" = 5490,
    "973" = 2972,
    "974" = 2975,
    "976" = 4471,
    "977" = 5490,
    "978" = 5490
  )
  if (num_departement %in% names(syst_proj)) {
    return(syst_proj[[num_departement]])
  } else {
    return(2154)
  }
}

# Fonction pour télécharger et traiter chaque département
telechargement_departement <- function(num_departement, num_annee, indic_parc = T) {
  
  mois_publi_ign <- c(
    "2024" = "07" # Modifier la date si nécessaire (association de l'année avec le mois de publication)
  )
  
  mois_publi_archive_pci_version_rec <- c(
    "2023" = "07" # Ajouter des dates si nécessaire
  )
  
  mois_publi_archive_pci_version_ant <- c(
    "2022" = "01",
    "2021" = "02",
    "2020" = "01",
    "2019" = "07"
  )

  # Construire l'URL du fichier .7z pour le département spécifié
  if (num_annee %in% names(mois_publi_ign)) {
    url <- paste0("https://data.geopf.fr/telechargement/download/PARCELLAIRE-EXPRESS/PARCELLAIRE-EXPRESS_1-1__SHP_",
                  sys_projection(num_departement),
                  "_D",
                  num_departement,
                  "_",
                  num_annee,
                  "-",
                  mois_publi_ign[num_annee],
                  "-01/PARCELLAIRE-EXPRESS_1-1__SHP_",
                  sys_projection(num_departement),
                  "_D",
                  num_departement,
                  "_",
                  num_annee,
                  "-",
                  mois_publi_ign[num_annee],
                  "-01.7z")
  }
  
  if(num_annee %in% names(mois_publi_archive_pci_version_rec)) {
    url <- paste0("https://files.opendatarchives.fr/professionnels.ign.fr/parcellaire-express/PARCELLAIRE_EXPRESS_1-1__SHP_",
                  sys_projection(num_departement),
                  "_D",
                  num_departement,
                  "_",
                  num_annee,
                  "-",
                  mois_publi_archive_pci_version_rec[num_annee],
                  "-01.7z")
  }
  
  if (num_annee %in% names(mois_publi_archive_pci_version_ant)) {
    url <- paste0("https://files.opendatarchives.fr/professionnels.ign.fr/parcellaire-express/PCI-par-DEPT_",
                  num_annee,
                  "-",
                  mois_publi_archive_pci_version_ant[num_annee],
                  "/PARCELLAIRE_EXPRESS_1-0__SHP_",
                  sys_projection(num_departement),
                  "_D",
                  num_departement,
                  "_",
                  num_annee,
                  "-",
                  mois_publi_archive_pci_version_ant[num_annee],
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
    print(str(parc))
    
  } else {
    print("Fichier non trouvé.")
  }
  
  files <- list.files(temp_dir, full.names = TRUE)
  # Filtrer les fichiers commencant par "PARCELLAIRE"
  parcellaire_file <- files[grep("^PARCELLAIRE", basename(files))]
  unlink(parcellaire_file, recursive = TRUE)
  return(parc)
}

# Fonction pour traiter les arrondissements et les cas de doublon de parcelles
traitement_doublon_et_arrondissement <- function(table_sf) {
  
  duplicates <- table_sf %>%
    group_by(IDU) %>%
    filter(n() > 1) %>%
    arrange(desc(FEUILLE))

  if (nrow(duplicates) > 0) {
    # Trouver la ligne avec la valeur maximale dans `feuille` pour chaque IDU
    to_remove <- duplicates %>%
      group_by(IDU) %>%
      slice(1) %>%
      ungroup()
    
    # Préparer le message d'alerte
    message <- paste("Doublon(s) supprimé(s):\n")
    for (i in 1:nrow(to_remove)) {
      row <- to_remove[i, ]
      message <- paste0(message, 
                        "IDU: ", row$IDU,
                        ", NUMERO: ", row$NUMERO,
                        ", FEUILLE: ", row$FEUILLE, 
                        ", SECTION: ", row$SECTION,
                        ", CODE_DEP: ", row$CODE_DEP,
                        ", NOM_COM: ", row$NOM_COM,
                        ", CODE_COM: ", row$CODE_COM,
                        ", COM_ABS: ", row$COM_ABS,
                        ", CODE_ARR: ", row$CODE_ARR,
                        ", CONTENANCE: ", row$CONTENANCE,
                        "\n")
      
      table_sf <- table_sf %>% 
        filter(!(IDU == row$IDU & FEUILLE == row$FEUILLE))
    }
    cat(message)
  }
  
  parc <- table_sf %>% 
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
      'geometry GEOMETRY(MULTIPOLYGON, ', code_sys_projection(num_departement),'));'
    ))
    
    dbExecute(conn,paste0(
      'CREATE INDEX idx_parc_', num_departement, '_', num_annee, '_geometry',
      ' ON parc_', num_departement, '_', num_annee,
      ' USING GIST(geometry);'))
    
    dbExecute(conn,paste0(
      'CREATE INDEX idx_parc_', num_departement, '_', num_annee, '_nom_com',
      ' ON parc_', num_departement, '_', num_annee,
      ' (nom_com);'))
    
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
      'geometry GEOMETRY(MULTIPOLYGON, ', code_sys_projection(num_departement),'));',
      collapse =''
    ))
    
    dbExecute(conn,paste0(
      'CREATE INDEX idx_com_', num_departement, '_geometry',
      ' ON com_', num_departement,
      ' USING GIST(geometry);'))
    
    dbExecute(conn,paste0(
      'CREATE INDEX idx_com_', num_departement, '_nom_com',
      ' ON com_', num_departement,
      ' (nom_com);'))
    
    # Remplissage ####
    sf::st_write(
      obj = table_sf %>% select(NOM_COM, CODE_INSEE) %>%  rename_with(tolower),
      dsn = conn,
      Id(table =  paste0('com_', num_departement)),
      append = TRUE
    )
  }
}