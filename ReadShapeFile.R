library(sf)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)
library(ggplot2)


#https://gis.stackexchange.com/questions/192993/where-can-i-find-state-borders-for-germany
#https://gadm.org/download_country_v3.html

#Map of Germany only:
mymap <- st_read("D:/GITHUB_REPOS/visualization-project2-smurfs/shapefiles/gadm36_DEU_shp/gadm36_DEU_0.shp",
                 stringsAsFactors = FALSE)
ggplot(mymap) + geom_sf()

#Map of Germany with states:
mymap <- st_read("D:/GITHUB_REPOS/visualization-project2-smurfs/shapefiles/gadm36_DEU_shp/gadm36_DEU_1.shp",
                 stringsAsFactors = FALSE)
#ggplot(mymap) + geom_sf() + geom_sf_label(aes(label = NAME_1))
mymap <- cbind(mymap, st_coordinates(st_centroid(mymap)))
ggplot(mymap) + geom_sf() + geom_text(data=mymap , aes(X,Y,label=NAME_1),size=2)

#https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html