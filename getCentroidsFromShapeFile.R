library(sf)
library(dplyr)
library(ggplot2)
DL_map <- st_read("~/Desktop/visualization-project2-smurfs/shapefiles/gadm36_DEU_shp/gadm36_DEU_2.shp",stringsAsFactors = FALSE)

centroids <- do.call(rbind,st_centroid(DL_map$geometry))
centroids <- as.data.frame(centroids)
centroids$landkreis <- DL_map$NAME_2
names(centroids) <- c("longitude" , "latitude","Landkreis_von_GS")
#https://github.com/r-spatial/sf/issues/75 (for extracting centroids from a shapefile)

#this plots the map with the centrids also:
ggplot() + geom_sf(data=DL_map) + geom_point(data=centroids,aes(x=longitude,y=latitude))

#see  https://gis.stackexchange.com/a/273382 for an approach which does not convert the centroids to a matrix and dataframe


require(kohonen)
require(RColorBrewer)

head(DL_MFH)

plot_som <- function(data) {
  total <- merge(data,centroids,by=c("Landkreis_von_GS"))
  
  expl_columns <- c('latitude', 'longitude', 'gebaeude_baujahr', 'gebaeude_nutzflaeche', 'verbrauch_gesamt_kwh_spez', 'abrechnungsjahr')
  subs <- total[, c('Landkreis_von_GS', expl_columns)]
  subs <-aggregate(subs[c(-1)], by = list(subs$Landkreis_von_GS), FUN=mean)
  
  
  
  old <- cur <- Inf
  for (i in 1:100){
    erg <- som(scale(subs[expl_columns]), grid = somgrid(6, 4, "hexagonal"))
    cur <- sum(erg$distances)
    if (cur<old){
      erg2 <- erg
      old <- cur
    }
  }
  plot.somgrid <- function (x, xlim, ylim, ...)
  {
    if (missing(xlim))
      xlim <- c(0, max(x$pts[, 1]) + min(x$pts[, 1]))
    if (missing(ylim))
    ylim <- c(max(x$pts[, 2]) + min(x$pts[, 2]), 0)
    plot(xlim, ylim, axes = FALSE, type = "n", xlab = "",
         ylab = "", asp=1, ...)
  }
  assignInNamespace("plot.somgrid", plot.somgrid, "kohonen")
  som2pts <- function(x){
    stopifnot("kohonen" %in% class(x))
    x$grid$pts[x$unit.classif,]
  }
  som_out <- som2pts(erg2)
  
  pal <- function(n) brewer.pal(n, "Set3")
  plot(erg2, shape="straight")
}
