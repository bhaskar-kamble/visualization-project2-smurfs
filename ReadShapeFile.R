library(sf)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)
library(ggplot2)


#https://gis.stackexchange.com/questions/192993/where-can-i-find-state-borders-for-germany
#https://gadm.org/download_country_v3.html

mymap <- st_read("D:/GITHUB_REPOS/visualization-project2-smurfs/shapefiles/gadm36_DEU_shp/gadm36_DEU_0.shp",
                 stringsAsFactors = FALSE)
ggplot(mymap) + geom_sf()

mymap <- st_read("D:/GITHUB_REPOS/visualization-project2-smurfs/shapefiles/gadm36_DEU_shp/gadm36_DEU_1.shp",
                 stringsAsFactors = FALSE)
ggplot(mymap) + geom_sf()

