#------------------------------------------------------------------------------------------------------#
###                                         LIBRARIES                                      #############
#------------------------------------------------------------------------------------------------------#
library(data.table)
library(geojsonio) #Import maps before processing as SpatialPolygonsDataFrame
library(ggplot2)
library(maptools) #From SPDF to sf. Needed for ggplot
library(rcartocolor) #Map colors
library(sf) #Read the Geojson files which do not require preprocess

library(tidyverse)
#------------------------------------------------------------------------------------------------------#
###                                       Bidding zones                                    #############
#------------------------------------------------------------------------------------------------------#

#Geojson files downloaded from https://github.com/fboerman/aten-geojson/releases/download/geojson-202110v2/geojson.zip

#Reading the files from the direcctory

setwd("C:/Users/Eve/Desktop/Shapefiles")
list_of_files <- list.files(path = ".", recursive = TRUE, #Read all geojson files in the wd
                            pattern = "\\.geojson$", 
                            full.names = TRUE)

Map<-do.call(rbind, sapply(list_of_files, sf::read_sf, simplify = FALSE)) #Create the Map of bidding zones

#Preprocess IT's, GB, DE-LU and BE

#Step 1: Read the Map and geojsonfiles
ITN<- geojson_read("./IT-NO.geojson",  what = "sp")
ITCN<- geojson_read("./IT-CNO.geojson",  what = "sp")
ITS<- geojson_read("./IT-SO.geojson",  what = "sp")
ITSC<- geojson_read("./IT-CSO.geojson",  what = "sp")
BE<-geojson_read("./BE.geojson",  what = "sp")
GB<-geojson_read("./GB.geojson",  what = "sp")
DE<-geojson_read("./DE.geojson",  what = "sp")
LU<-geojson_read("./LU.geojson",  what = "sp")
Delu<-raster::aggregate(rbind(DE, LU))

#Modify geojson files for getting only one bidding-zone
#Warning messages are shown, this time they are not much important for the final result

Map[2,3]<-st_as_sf(
  unionSpatialPolygons(BE,rep(1, nrow(BE)))
)

Map[13,3]<-st_as_sf(
  unionSpatialPolygons(ITCN,rep(1, nrow(ITCN)))
)

Map[14,3]<-st_as_sf(
  unionSpatialPolygons(ITSC,rep(1, nrow(ITSC)))
)

Map[15,3]<-st_as_sf(
  unionSpatialPolygons(ITN,rep(1, nrow(ITN)))
)

Map[16,3]<-st_as_sf(
  unionSpatialPolygons(ITS,rep(1, nrow(ITS)))
)

Map[12,3]<-st_as_sf(
  unionSpatialPolygons(GB,rep(1, nrow(GB)))
)

#Step 2: Eliminate LU and Create DE-LU zone

Map<-Map[-18,]
Map[5,3]<-st_as_sf(
  unionSpatialPolygons(Delu,rep(1, nrow(Delu)))
)

Map$id<- c("AUT", "BEL", "CHE","CZE","DE-LU","DNK1", "DNK2", "EST", "ESP","FIN", "FRA", "GBR","ITA-CN", "ITA-CS", "ITA-N", "ITA-S", 
           "LTU", "LVA", "NLD", "NOR1", "NOR2", "NOR3", "NOR4", "NOR5", "POL", "PRT", "SWE")

