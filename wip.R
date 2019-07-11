setwd('~/Desktop/visualization-project2-smurfs')


# 1.

mfh_area <- read.csv('MFHAreas_bundeslands.csv')
sfh_area <- read.csv('SFHAreas_bundeslands.csv')


library(tidyr)
average_by_year_bundesland <- function(data) {
  
  average_consumption_mfh <- aggregate(data[,c('abrechnungsjahr', 'bundesland', 'verbrauch_gesamt_kwh_spez')][c(-1, -2)], 
                                       by=list(data$abrechnungsjahr, data$bundesland), FUN=sum)
  
  colnames(average_consumption_mfh) <- c('abrechnungsjahr', 'bundesland', 'verbrauch_gesamt_kwh_spez')
  
  return(average_consumption_mfh)
}
average_by_year_bundesland_energietrager <- function(data) {
  
  avg_bundesland_year <- average_by_year_bundesland(data)
  data_aggregated <- aggregate(data[,c('abrechnungsjahr', 'bundesland', 'energietraeger', 'verbrauch_gesamt_kwh_spez')][c(-1, -2, -3)], 
                                       by=list(data$abrechnungsjahr, data$bundesland, data$energietraeger), FUN=sum)
  
  colnames(data_aggregated) <- c('abrechnungsjahr', 'bundesland', 'energietraeger', 'verbrauch_gesamt_kwh_spez')

  data_aggregated <- as.data.frame(complete(data_aggregated, abrechnungsjahr, energietraeger, bundesland, fill = list(verbrauch_gesamt_kwh_spez = 0)))
  
  for (year in c(2002:2018)) {
    for (state in unique(data_aggregated$bundesland)) {
      for (energietraeger in unique(data_aggregated$energietraeger)) {
           data_aggregated[data_aggregated$abrechnungsjahr == year & data_aggregated$bundesland == state & data_aggregated$energietraeger == energietraeger, ]$verbrauch_gesamt_kwh_spez <- 
        data_aggregated[data_aggregated$abrechnungsjahr == year & data_aggregated$bundesland == state & data_aggregated$energietraeger == energietraeger, ]$verbrauch_gesamt_kwh_spez / 
        avg_bundesland_year[avg_bundesland_year$abrechnungsjahr == year & avg_bundesland_year$bundesland == state, ]$verbrauch_gesamt_kwh_spez
           }
    }
  }
  
  data_wide <- spread(data_aggregated, energietraeger, verbrauch_gesamt_kwh_spez)
  data_wide
  
  return(as.data.frame(data_wide))
}

head(average_by_year_bundesland_energietrager(DL_MFH))