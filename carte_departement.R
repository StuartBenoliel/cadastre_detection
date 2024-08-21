library(sf)
library(dplyr)
library(mapview)
library(DBI)
library(ggplot2)
library(webshot)
library(stringr)
rm(list = ls())

params <- list(
  num_departement = "23",
  temps_apres = 23,
  temps_avant = 22
) # A modifier au besoin

source(file = "database/connexion_db.R")
conn <- connecter()

dbExecute(conn, paste0(
  "SET search_path TO traitement_", params$temps_apres, "_", params$temps_avant, "_cadastre_" , params$num_departement, 
  ", cadastre_", params$num_departement, ", public"
))

bordure <- st_read(conn, query = "SELECT * FROM bordure;")
contour_commune <- st_read(conn, query = paste0(
  "SELECT nom_com, code_insee, ST_Boundary(geometry) AS geometry FROM com_", params$num_departement, ";"))

parc_avant <- st_read(conn, query = paste0("SELECT * FROM parc_", params$num_departement, "_", params$temps_avant, ";"))
parc_apres <- st_read(conn, query = paste0("SELECT * FROM parc_", params$num_departement, "_", params$temps_apres, ";"))

contour <- st_read(conn, query = "SELECT * FROM contour;")
translation <- st_read(conn, query = "SELECT * FROM translation;")
redecoupage <- st_read(conn, query = "SELECT * FROM redecoupage;")
contour_redecoupage <- st_read(conn, query = "SELECT * FROM contour_redecoupage;")
vrai_ajout <- st_read(conn, query = "SELECT * FROM vrai_ajout;")
vrai_supp <- st_read(conn, query = "SELECT * FROM vrai_supp;")

scission_com <- st_read(conn, query = "SELECT * FROM scission_com;")
fusion_com <- st_read(conn, query = "SELECT * FROM fusion_com;")

ajout <- st_read(conn, query = "SELECT * FROM ajout;")
supp <- st_read(conn, query = "SELECT * FROM supp;")
modif_avant <- st_read(conn, query = "
                       SELECT idu, nom_com, code_com, com_abs, contenance_avant,
                            iou, iou_recale, iou_multi, participants_avant, 
                            participants_apres, geometry_avant FROM modif;")

modif_apres <- st_read(conn, query = "
                       SELECT idu, nom_com, code_com, com_abs, contenance_apres,
                            iou, iou_recale, iou_multi, participants_avant, 
                            participants_apres, geometry_apres FROM modif;")

map <- mapview(bordure, 
                    layer.name = "Bordures étendues", col.regions = "#F2F2F2", 
                    alpha.regions = 0.7, homebutton = F,
                    map.types = c("CartoDB.Positron", "OpenStreetMap", "Esri.WorldImagery")) +
  mapview(contour_commune, color = "black",
          layer.name = "Contour communal",
          homebutton = F,
          legend = FALSE)

if (nrow(translation) > 0) {
  map <- map + mapview(translation,
                       layer.name = paste0("Parcelles translatées (état 20",params$temps_apres,")"), 
                       col.regions = "#069F9C",
                       alpha.regions = 0.5, homebutton = F) +
    mapview(parc_avant %>%
              filter(idu %in% translation$idu_recale),
            col.regions = "#069F9C",
            layer.name = paste0("Parcelles translatées (état 20",params$temps_avant,")"), 
            alpha.regions = 0.5, homebutton = F)
}
if (nrow(contour) > 0) {
  map <- map + mapview(parc_apres %>%
                         filter(idu %in% unlist(str_split(contour$participants_apres, ",\\s*"))),  
                       layer.name = paste0("Parcelles contours (état 20",params$temps_apres,")"), 
                       col.regions = "#D79700", alpha.regions = 0.5, homebutton = F)  +
    mapview(contour,  
            layer.name = paste0("Parcelles contours (état 20",params$temps_avant,")"),
            col.regions = "#D79700",
            alpha.regions = 0.5, homebutton = F)
}
if (nrow(redecoupage) > 0) {
  map <- map + mapview(parc_apres %>%
                         filter(idu %in% unlist(str_split(redecoupage$participants_apres, ",\\s*"))),
                       layer.name = paste0("Parcelles redécoupées (état 20",params$temps_apres,")"),
                       col.regions = "#AE48C0",
                       alpha.regions = 0.5, homebutton = F) +
    mapview(redecoupage,  
            layer.name = paste0("Parcelles redécoupées (état 20",params$temps_avant,")"), 
            col.regions = "#AE48C0", alpha.regions = 0.5, homebutton = F)
  
}
if (nrow(contour_redecoupage) > 0) {
  map <- map + mapview(parc_apres %>%
                         filter(idu %in% unlist(str_split(contour_redecoupage$participants_apres, ",\\s*"))),  
                       layer.name = paste0("Parcelles redecoupage + contours (état 20",params$temps_apres,")"),
                       col.regions = "#FFB9BB",
                       alpha.regions = 0.5, homebutton = F) +
    mapview(contour_redecoupage,
            layer.name = paste0("Parcelles redecoupage + contours (état 20",params$temps_avant,")"), 
            col.regions = "#FFB9BB", alpha.regions = 0.5, homebutton = F)
}
if (nrow(scission_com) > 0) {
  map <- map + mapview(scission_com,  
                       layer.name = paste0("Parcelles scission de communes (état 20",params$temps_apres,")"), 
                       col.regions = "#F5F5F5",
                       alpha.regions = 0.5, homebutton = F) +
    mapview(parc_avant %>%
              filter(idu %in% scission_com$idu_avant),  
            layer.name = paste0("Parcelles scission de communes (état 20",params$temps_avant,")"), 
            col.regions = "#F5F5F5", alpha.regions = 0.5, homebutton = F)
}
if (nrow(fusion_com) > 0) {
  map <- map + mapview(fusion_com,  
                       layer.name = paste0("Parcelles fusion de communes (état 20",params$temps_apres,")"), 
                       col.regions = "white",
                       alpha.regions = 0.5, homebutton = F) +
    mapview(parc_avant %>%
              filter(idu %in% fusion_com$idu_avant),  
            layer.name = paste0("Parcelles fusion de communes (état 20",params$temps_avant,")"), 
            col.regions = "white", alpha.regions = 0.5, homebutton = F)
}
if (nrow(vrai_ajout) > 0) {
  map <- map + mapview(vrai_ajout, 
                       layer.name = "Parcelles véritablement ajoutées", 
                       col.regions = "#26A44B", alpha.regions = 0.5, homebutton = F)
  
}
if (nrow(vrai_supp) > 0) {
  map <- map + mapview(vrai_supp,
                       layer.name = "Parcelles véritablement supprimées",
                       col.regions = "#E91422", alpha.regions = 0.5, homebutton = F) 
}
if (nrow(ajout) > 0) {
  map <- map + mapview(ajout,
                       z = c("iou"), layer.name = paste0("Parcelles restantes (état 20",params$temps_apres,")"), 
                       alpha.regions = 0.5, homebutton = F)
}
if (nrow(supp) > 0) {
  map <- map + mapview(supp, 
                       z = c("iou_multi"), layer.name = paste0("Parcelles restantes (état 20",params$temps_avant,")"), 
                       alpha.regions = 0.5, homebutton = F)
}
if (nrow(modif_apres) > 0) {
  map <- map + mapview(modif_apres, 
                       z = c("iou_recale"), 
                       layer.name = paste0("Parcelles modifiées restantes (état 20",params$temps_apres,")"), 
                       alpha.regions = 0.5, homebutton = F)
  
}
if (nrow(modif_avant) > 0) {
  map <- map + mapview(modif_avant, 
                       z = c("iou_recale"), 
                       layer.name = paste0("Parcelles modifiées restantes (état 20",params$temps_avant,")"), 
                       alpha.regions = 0.5, homebutton = F)
}

mapshot(map, url = paste0("parcelles_",params$num_departement,"_",params$temps_apres,"-",params$temps_avant,".html"))