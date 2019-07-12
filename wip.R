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




#################################################################################################

mfh_area <- read.csv2('MFHAreas_bundeslands.csv')
sfh_area <- read.csv2('SFHAreas_bundeslands.csv')
all_area <- read.csv2('Areas_SFH_MFH.csv')
co2_coef <- read.csv('Germany_CO2_coefficients.txt')
states
convert_area_to_co2_emissions<- function(data, energy_consumption_data, co2_coef) {
  
  et <- unique(energy_consumption_data$energietraeger)
  averages <- average_by_year_bundesland_energietrager(energy_consumption_data)
  names(data) <- c('abrechnungsjahr', states)
  names(co2_coef) <- tolower(names(co2_coef))
  
  data <- gather(data, bundesland, area, states, factor_key=TRUE)
  data$area <- as.numeric(data$area)
  for (et_t in et) {
    data[[et_t]] <- rep(NA, (16 * 17))
  }
  for (year in c(2002:2018)) {
    for (state in unique(energy_consumption_data$bundesland)) {
      for (et_t in et) {
        data[data$abrechnungsjahr == year & data$bundesland == state,][[et_t]] <-  
          data[data$abrechnungsjahr == year & data$bundesland == state,]$area
          averages[averages$abrechnungsjahr == year & averages$bundesland == state,][[et_t]] *
          co2_coef[co2_coef$jahr == year,][[et_t]]
      }
    }
  }
  
  data
}
head(convert_area_to_co2_emissions(mfh_area, DL_MFH, co2_coef))
