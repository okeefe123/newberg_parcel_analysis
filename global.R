# global.R
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(geosphere)

pkgs<- c("scales", "shiny", "data.table", "magrittr",
         "lubridate", "leaflet", "sf", "DBI", "pool", "jsonlite", "httr", "base64enc", "digest", "filesstrings", "readxl", "highcharter", "pdftools", "stringdist", "DT", "htmlwidgets", "geosphere")
sapply(pkgs, require, character.only = TRUE)
# ipak <- function(pkg){
#   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
#   if (length(new.pkg)) 
#     install.packages(new.pkg, dependencies = TRUE)
#   sapply(pkg, require, character.only = TRUE)
# }
# 
# ipak(pkgs)


m0<- leaflet::leaflet() %>%
  leaflet::addProviderTiles(providers$CartoDB.Positron, group ="CartoDB.Positron", options = providerTileOptions(detectRetina = T, minZoom = 2, maxZoom = 18)) %>%
  leaflet::addProviderTiles(providers$Esri.WorldImagery, group = "Satellite", options = providerTileOptions(detectRetina = T)) %>%
  leaflet::addProviderTiles(providers$CartoDB.PositronOnlyLabels, group="Labels", options = providerTileOptions(detectRetina = T)) %>%
  leaflet::addLayersControl(
    baseGroups = c("CartoDB.Positron", "Satellite"),
    overlayGroups = c("Labels"),
    options = leaflet::layersControlOptions(collapsed = TRUE))