# Créer une base de données postgis avec la plateforme datalab-sspcloud 

library(DBI)
library(RPostgres)
library(dplyr)
library(dbplyr)
library(sf)

# 0- Créer un service postgres avec l'extension postgis sur le datalab ####

# a-Pour cela, vous pouvez utiliser directement l'URL suivante:
# https://datalab.sspcloud.fr/launcher/databases/postgresql?autoLaunch=false&onyxia.friendlyName=«tp-ensai-postgis-perso-db»&postgresql.extensions.postgis=true

# Puis lancer le service

# b- Récupérez les infos de connexion ####
# Sur la page d'accueil de vos services, récupérez les informations suivantes
# (cliquez sur README du service postgis que vous venez de créer)
# et remplissez les dans le fichier connexion_db.R

# - name_database 
# - user_name
# - password 
# - url : conservez uniquement la partie de l'url entre les // et les :
# - port : partie de l'url après les :

# 1- Connexion à la base de données ####
source("connexion_db.R", encoding = "utf-8")
conn <- connecter()

DBI::dbListObjects(conn)

# 2- Ajout de la Base Permanente des Equipements (BPE) ####

# La base est téléchargeable sur le site insee.fr:
# "https://www.insee.fr/fr/statistiques/fichier/3568638/bpe21_ensemble_xy_csv.zip"


# a- Importation depuis Minio ####

# Pour soulager le réseau, la base a été chargée sur un espace public du datalab-sspcloud (Minio).
# Ces serveurs étant ouverts à l'internet, on veillera à n'y déposer que des données non confidentielles.

file_bpe<-"bpe21_ensemble_xy.csv"
download.file("https://julienjamme.minio.lab.sspcloud.fr/public/bpe21_ensemble_xy.csv", file_bpe)
bpe21 <- readr::read_delim(file_bpe,
                           delim = ";",
                           col_types = readr::cols(LAMBERT_X="n",LAMBERT_Y="n",.default="c"))


# ALTERNATIVE: en téléchargeant directement la BPE 2021 depuis internet
# temp <- tempfile()
# download.file("https://www.insee.fr/fr/statistiques/fichier/3568638/bpe21_ensemble_xy_csv.zip",temp)
# bpe21 <- readr::read_delim(unz(temp, "bpe21_ensemble_xy.csv"), delim = ";", col_types = readr::cols(LAMBERT_X="n",LAMBERT_Y="n",.default="c"))
# str(bpe21)
# unlink(temp)

table(is.na(bpe21$LAMBERT_X))
table(is.na(bpe21$LAMBERT_Y))

# L'objet est un data.frame que nous transformons en objet spatial:
# Chaque équipement est transformé en un point dont la localisation est fournie par 
# les variables LAMBERT_X et LAMBERT_Y

# Comme en métropole et dans les doms, le système de projection n'est pas identique
# On crée autant d'objets qu'il y a de projections différentes

# Un objet pour la métropole
sf_bpe21_metro <- bpe21 %>% 
  na.omit() %>% 
  filter(REG > 10) %>% 
  mutate(id = 1:n()) %>% 
  relocate(id) %>% 
  sf::st_as_sf(coords = c("LAMBERT_X","LAMBERT_Y"), crs = 2154)

str(sf_bpe21_metro)  

# Quatre objets pour les DOM (1 pour chaque)
sf_bpe21_doms <- purrr::map2(
  c("01","02","03","04"), #pas de données géolocalisées sur Mayotte
  c(5490,5490,2972,2975),
  function(reg,epsg){
    bpe21 %>% 
      filter(REG == reg) %>% 
      na.omit() %>% 
      mutate(id = 1:n()) %>% 
      relocate(id) %>% 
      sf::st_as_sf(coords = c("LAMBERT_X","LAMBERT_Y"), crs = epsg)
  }
)
names(sf_bpe21_doms) <- c("01","02","03","04")

# c- Construction et Exécution des requêtes pour créer les données sur la base postgis ####

# Métropole: Construction de la requête créant les tables ####
types_vars_metro <- purrr::map_chr(
  names(sf_bpe21_metro)[-c(1,24)],
  function(var){
    paste0(var, " VARCHAR(", max(nchar(sf_bpe21_metro[[var]])), "), ")
  }
) %>% 
  paste0(., collapse="")

query <- paste0(
  'CREATE TABLE bpe21_metro',
  '( id INT PRIMARY KEY,',
  types_vars_metro,
  'geometry GEOMETRY(POINT, 2154));',
  collapse =''
)

# Métropole: Création de la table (structure vide) ####
dbSendQuery(conn, query)
dbListTables(conn)

# Métropole: Remplissage avec la table metro ####
sf::st_write(
  obj = sf_bpe21_metro %>% rename_with(tolower),
  dsn = conn,
  Id(table = 'bpe21_metro'),
  append = TRUE
)

# test lecture
bpe_head <- sf::st_read(conn, query = 'SELECT * FROM bpe21_metro LIMIT 10;')
str(bpe_head)
st_crs(bpe_head)

dbDisconnect(conn)

# DOMS: Construction de la requête créant les tables ####
purrr::walk2(
  c("01","02","03","04"), # codes des différents départements
  c(5490,5490,2972,2975), # codes epsg (projection) spécifiques à chaque dom
  function(reg,epsg){
    # création de la requête
    nom_table <- paste0('bpe21_',reg)
    types_vars <- purrr::map_chr(
      names(sf_bpe21_doms[[reg]])[-c(1,24)],
      function(var){
        paste0(var, " VARCHAR(", max(nchar(sf_bpe21_doms[[reg]][[var]])), "), ")
      }
    ) %>% 
      paste0(., collapse="")
    
    query <- paste0(
      'CREATE TABLE ',nom_table,
      '( id INT PRIMARY KEY, ',
      types_vars,
      'geometry GEOMETRY(POINT, ', epsg,'));',
      collapse =''
    )
    # Exécution de la requête créant la table
    conn <- connecter()
    # Remplissage de la table avec les données
    sf::st_write(
      obj = sf_bpe21_doms[[reg]] %>% rename_with(tolower),
      dsn = conn,
      Id(table = nom_table),
      append = FALSE
    )
    
    # query <- paste0(
    # "UPDATE geometry_columns 
    # SET srid = ", epsg, ", type = 'POINT'
    # WHERE f_table_name='",nom_table, "';")
    # dbSendQuery(conn, query)
    
    dbDisconnect(conn)
  }
)

# Vérification de la présence de toutes les tables dans la base de données
conn <- connecter()
dbListTables(conn)

# Vérification que les données sont bien présentes 
bpe_head <- sf::st_read(conn, query = 'SELECT * FROM bpe21_04 LIMIT 10;')
str(bpe_head)
st_crs(bpe_head)

# Déconnexion
dbDisconnect(conn)


# 3- Ajout d'une table communale ####

# a - Import des données depuis le cloud Minio ####
# Table de population communale
file_temp <- "BTT_TD_POP1A_2018.CSV"
download.file("https://julienjamme.minio.lab.sspcloud.fr/public/BTT_TD_POP1A_2018.CSV",file_temp)
popcom <- readr::read_delim(file_temp,
                            delim = ";",
                            col_types = list(NB="n",.default="c"))
str(popcom)

# Table des naissances
file_temp <- "base_naissances_2021.csv"
download.file("https://julienjamme.minio.lab.sspcloud.fr/public/base_naissances_2021.csv",file_temp)
naisscom<- readr::read_delim(file_temp,
                             delim = ";")
str(naisscom)


# Autre solution Import via téléchargement direct:
# temp <- tempfile()
# download.file("https://www.insee.fr/fr/statistiques/fichier/5395878/BTT_TD_POP1A_2018.zip",temp)
# popcom <- readr::read_delim(unz(temp, "BTT_TD_POP1A_2018.CSV"), delim = ";",col_types = list(NB="n",.default="c"))
# str(popcom)
# unlink(temp)
# 
# temp <- tempfile()
# download.file("https://www.insee.fr/fr/statistiques/fichier/1893255/base_naissances_2021_csv.zip",temp)
# naisscom <- readr::read_delim(unz(temp, "base_naissances_2021.csv"), delim = ";")
# str(naisscom)
# unlink(temp)

# b- Traitements sur les données ####
# Répartition des naissances communales dans les arrondissements municipaux au prorata des naissances déjà observées

naiss_paris <- naisscom %>% filter(CODGEO == "75056") %>% pull(NAISD18)
paris_ratio <- naisscom %>% 
  filter(substr(CODGEO,1,3) == 751) %>% 
  select(CODGEO, NAISD18) %>% 
  mutate(ratio = NAISD18/sum(NAISD18)) %>% 
  mutate(NAISD18_ADD = naiss_paris*ratio)

naiss_mars <- naisscom %>% filter(CODGEO == "13055") %>% pull(NAISD18)
mars_ratio <- naisscom %>% 
  filter(substr(CODGEO,1,3) == 132) %>% 
  select(CODGEO, NAISD18) %>% 
  mutate(ratio = NAISD18/sum(NAISD18)) %>% 
  mutate(NAISD18_ADD = naiss_mars*ratio)

naiss_lyon <- naisscom %>% filter(CODGEO == "69123") %>% pull(NAISD18)
lyon_ratio <- naisscom %>% 
  filter(substr(CODGEO,1,4) == 6938) %>% 
  select(CODGEO, NAISD18) %>% 
  mutate(ratio = NAISD18/sum(NAISD18)) %>% 
  mutate(NAISD18_ADD = naiss_lyon*ratio)

popnaiss_com <- popcom %>% 
  group_by(CODGEO) %>%
  summarise(POP = sum(NB), .groups = 'drop') %>% 
  full_join(
    popcom %>% 
      group_by(CODGEO, SEXE) %>%
      summarise(NB = sum(NB), .groups = 'drop') %>% 
      mutate(SEXE = case_when(SEXE==1~"SEXE_H",TRUE~"SEXE_F")) %>% 
      tidyr::pivot_wider(names_from = SEXE, values_from = NB, values_fill = 0),
    by = "CODGEO"
  ) %>% 
  full_join(
    popcom %>% 
      group_by(CODGEO, AGEPYR10) %>%
      summarise(NB = sum(NB), .groups = 'drop') %>% 
      mutate(AGEPYR10 = paste0("AGE_", ifelse(as.numeric(AGEPYR10) < 10, paste0("0",AGEPYR10), AGEPYR10))) %>% 
      arrange(CODGEO, AGEPYR10) %>% 
      tidyr::pivot_wider(names_from = AGEPYR10, values_from = NB, values_fill = 0),
    by = "CODGEO"
  ) %>%
  left_join(
    popcom %>%  select(1:3) %>% unique(), by = "CODGEO"
  ) %>% 
  relocate(CODGEO, NIVGEO, LIBGEO) %>% 
  full_join(
    naisscom %>% 
      full_join(
        bind_rows(bind_rows(paris_ratio, mars_ratio), lyon_ratio)
      ) %>% 
      tidyr::replace_na(list(NAISD18_ADD = 0)) %>% 
      mutate(NAISD18_AJ = NAISD18 + NAISD18_ADD) %>% 
      select(CODGEO, NAISD18_AJ), 
    by = "CODGEO"
  ) %>% 
  filter(! CODGEO %in% c("75056","13055","69123")) %>% 
  filter(substr(CODGEO,1,2) < 97) %>% 
  filter(!is.na(NAISD18_AJ)) %>%  # pb coherence geo entre les deux tables (11 communes concernées)
  rename_with(tolower)

# c- Ecriture de la table sur la base de données ####
conn <- connecter()
dbWriteTable(conn, "popnaiss_com", popnaiss_com, row.names=F)

# Vérification que la table a bien été créée
dbListTables(conn)

# Vérification des données 
poptest <- dbGetQuery(conn, "SELECT * FROM popnaiss_com LIMIT 10;")
str(poptest)
poptest2 <- dbGetQuery(conn, "SELECT * FROM popnaiss_com WHERE codgeo='35238';")
str(poptest2)
dbDisconnect(conn)

# 4- Ajout de fonds de polygones ####

# a- Recupération du fond régional sur Minio ####

file <- "reg_francemetro_2021.gpkg"
download.file("https://julienjamme.minio.lab.sspcloud.fr/public/reg_francemetro_2021.gpkg",file)
sf_reg_metro <- sf::st_read(file)

sf_reg_metro %>% str()
st_crs(sf_reg_metro)

# b- Rédaction de la requête pour créer une table ####
query <-
  'CREATE TABLE regions_metro(
  surf INTEGER,
  code CHAR(2) PRIMARY KEY,
  libelle VARCHAR(30),
  geometry GEOMETRY(MULTIPOLYGON, 2154));'

# c- Exécution de la requête ####
conn <- connecter()
dbSendQuery(conn, query)
dbListTables(conn)

# d- Remplissage de la table ####
sf::st_write(
  obj = sf_reg_metro %>% rename_with(tolower),
  dsn = conn,
  Id(table = 'regions_metro'),
  append = TRUE
)

# Vérification que la table se charge bien
st_read(conn, query = "SELECT * FROM regions_metro;") %>% st_geometry() %>% plot()

# Déconnexion finale
dbDisconnect(conn)
