library(sf)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)
library(ggplot2)




#Map of Germany with states:
mymap <- st_read("D:/GITHUB_REPOS/visualization-project2-smurfs/shapefiles/gadm36_DEU_shp/gadm36_DEU_1.shp",
                 stringsAsFactors = FALSE)
mymap$value <- 1:16
#mymap <- cbind(mymap, st_coordinates(st_centroid(mymap)))
#mymap <- st_transform(crs="+proj=laea +lon_0=18.984375")



#https://www.youtube.com/watch?v=GMi1ThlGFMo
ggplot(mymap) + geom_sf(aes(fill=value)) #(with ggplot)
#with tmap:
tm_shape(mymap) + tm_polygons("value",id="NAME_1",style="cont")
#tm_shape(mymap) + tm_fill("value",id="NAME_1")
#tm_shape(mymap) + tm_borders("value",id="NAME_1")
tmap_mode("view")
tmap_last()
#https://geocompr.robinlovelace.net/adv-map.html 
#https://stackoverflow.com/questions/50514651/how-to-use-a-continuous-color-scale-for-thematic-maps-in-the-r-package-tmap

#http://zevross.com/blog/2018/10/02/creating-beautiful-demographic-maps-in-r-with-the-tidycensus-and-tmap-packages/





library(leaflet)
leaflet() %>% 
  addPolygons(data = mymap,
              color="white")
#https://civisanalytics.github.io/interactive-map-tutorial/
#http://smartdigiag.com/2018/01/InteractiveMapping

#ggiraph from https://bhaskarvk.github.io/user2017.geodataviz/notebooks/03-Interactive-Maps.nb.html
#https://github.com/bhaskarvk/user2017.geodataviz/blob/master/inst/extdata/africa-internet_usage-2015.csv
library(ggiraph)
ggplot(mymap) + geom_polygon_interactive(color="black")#+ geom_sf()+
ggplot(mymap) + geom_polygon_interactive(color="black",aes())

############################################

#https://rstudio-pubs-static.s3.amazonaws.com/441859_f49887e4cf654f09aa4d48bb2f4cce09.html
world <- sf::st_as_sf(rnaturalearth::countries110)



#https://stackoverflow.com/questions/48044543/custom-tooltip-in-ggplot-using-geom-polygoncoord-map
counties <- map_data("county")