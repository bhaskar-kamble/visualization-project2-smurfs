library(sf)
library(dplyr)
library(ggplot2)
DL_map <- st_read(
  "D:/GITHUB_REPOS/visualization-project2-smurfs/shapefiles/gadm36_DEU_shp/gadm36_DEU_2.shp",stringsAsFactors = FALSE
)

centroids <- do.call(rbind,st_centroid(DL_map$geometry))
centroids <- as.data.frame(centroids)
names(centroids) <- c("longitude" , "latitude")
#https://github.com/r-spatial/sf/issues/75 (for extracting centroids from a shapefile)

#this plots the map with the centrids also:
ggplot() + geom_sf(data=DL_map) + geom_point(data=centroids,aes(x=longitude,y=latitude))

#see  https://gis.stackexchange.com/a/273382 for an approach which does not convert the centroids to a matrix and dataframe