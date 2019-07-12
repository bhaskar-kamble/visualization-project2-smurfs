
mfh_area <- read.csv2('MFHAreas_bundeslands.csv')
sfh_area <- read.csv2('SFHAreas_bundeslands.csv')
all_area <- read.csv2('Areas_SFH_MFH.csv')
co2_coef <- read.csv('Germany_CO2_coefficients.txt')


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
  
  return(as.data.frame(data_wide))
}

average_by_year <- function(data) {
  
  average_consumption_mfh <- aggregate(data[,c('abrechnungsjahr',  'verbrauch_gesamt_kwh_spez')][c(-1)], 
                                       by=list(data$abrechnungsjahr), FUN=sum)
  
  colnames(average_consumption_mfh) <- c('abrechnungsjahr', 'verbrauch_gesamt_kwh_spez')
  
  return(average_consumption_mfh)
}


##############################################
average_by_year_energietrager <- function(data) {
  
  avg_bundesland_year <- average_by_year(data)
  data_aggregated <- aggregate(data[,c('abrechnungsjahr', 'energietraeger', 'verbrauch_gesamt_kwh_spez')][c(-1, -2)], 
                               by=list(data$abrechnungsjahr, data$energietraeger), FUN=sum)
  
  colnames(data_aggregated) <- c('abrechnungsjahr', 'energietraeger', 'verbrauch_gesamt_kwh_spez')
  
  data_aggregated <- as.data.frame(complete(data_aggregated, abrechnungsjahr, energietraeger, fill = list(verbrauch_gesamt_kwh_spez = 0)))
  
  for (year in c(2002:2018)) {
    for (energietraeger in unique(data_aggregated$energietraeger)) {
      data_aggregated[data_aggregated$abrechnungsjahr == year & data_aggregated$energietraeger == energietraeger, ]$verbrauch_gesamt_kwh_spez <- 
        data_aggregated[data_aggregated$abrechnungsjahr == year & data_aggregated$energietraeger == energietraeger, ]$verbrauch_gesamt_kwh_spez / 
        avg_bundesland_year[avg_bundesland_year$abrechnungsjahr == year, ]$verbrauch_gesamt_kwh_spez
    }
  }
  
  data_wide <- spread(data_aggregated, energietraeger, verbrauch_gesamt_kwh_spez)
  
  return(as.data.frame(data_wide))
}


convert_area_to_co2_emissions_all<- function(data, energy_consumption_data, co2_coef) {
  
  et <- unique(energy_consumption_data$energietraeger)
  averages <- average_by_year_energietrager(energy_consumption_data)
  averages <- averages[-17,]
  
  
  names(co2_coef) <- tolower(names(co2_coef))
  if (length(unique(energy_consumption_data$gtype))  == 2) {
    data <- data.frame(abrechnungsjahr = data$Jahr, area = (data$AreaMFH + data$AreaSFH))
  }
  else if (unique(energy_consumption_data$gtype) == "MFH") {
    data <- data.frame(abrechnungsjahr = data$Jahr, area = data$AreaMFH)
  }
  else if (unique(energy_consumption_data$gtype) == "SFH") {
    data <- data.frame(abrechnungsjahr = data$Jahr, area = data$AreaSFH)
  }
  for (et_t in et) {
    data[[et_t]] <- rep(NA, 16)
  }
  for (year in c(2002:2018)) {
    for (et_t in et) {
      data[data$abrechnungsjahr == year,][[et_t]] <-  
        data[data$abrechnungsjahr == year,]$area * 1000 *
        averages[averages$abrechnungsjahr == year,][[et_t]] *
        co2_coef[co2_coef$jahr == year,][[et_t]]
    }
  }
  
  data$erdgas <- data$erdgas / 10^2
  data$fluessiggas <- data$fluessiggas / 10^2
  data$heizoel <- data$heizoel / 10^2
  data$strom <- data$strom / 10^2
  data$holzpellets <- data$holzpellets / 10^2
  data$waerme <- data$waerme / 10^2
  
  data
  
}


#################################################################################################


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
          data[data$abrechnungsjahr == year & data$bundesland == state,]$area * 1000 *
          averages[averages$abrechnungsjahr == year & averages$bundesland == state,][[et_t]] *
          co2_coef[co2_coef$jahr == year,][[et_t]]
      }
    }
  }
  
  data$erdgas <- data$erdgas / 10^2
  data$fluessiggas <- data$fluessiggas / 10^2
  data$heizoel <- data$heizoel / 10^2
  data$strom <- data$strom / 10^2
  data$holzpellets <- data$holzpellets / 10^2
  data$waerme <- data$waerme / 10^2
  
  data
}

get_visibility_combinations_co2 <- function() {
  visibility_combinations <- list()
  i = 1
  labels = c('ALL', 'MFH', 'SFH')
  for (label in labels) {
    to = 3 * 6 + 1
    vis <- rep(F, to)
    for(k in ((i - 1) * 6 + 2): (i * 6 + 1))
      vis[k] <- TRUE
    visibility_combinations[[i]] <- list(method = "restyle",
                                         args = list("visible", as.list(vis)),
                                         label = label)
    i = i + 1
  }
  return(visibility_combinations)
}


#########################################################################################

<<<<<<< HEAD
get_map_data_co2 <- function(year) {
=======
#get_map_data_co2() {
#  enegietraeger <- unique(DL_MFH$energietraeger)
#  convert_area_to_co2_emissions(mfh_area,DL_MFH,co2_coef)
#}

get_map_data_co2 <- function() {
>>>>>>> a720390a950e1cf14be44f8f88afec3387a3e506
  enegietraeger <- unique(DL_MFH$energietraeger)
  mfh <- convert_area_to_co2_emissions(mfh_area,DL_MFH,co2_coef)
  mfh$mean <- apply(mfh[enegietraeger], 1, sum)
  mfh <- data.frame(abrechnungsjahr = mfh$abrechnungsjahr, bundesland = mfh$bundesland, 
                    mean = mfh$mean)[abrechnungsjahr == year,]

  sfh <- convert_area_to_co2_emissions(sfh_area, DL_SFH, co2_coef)
  sfh$mean <- apply(sfh[unique(DL_SFH$energietraeger)], 1, sum)
  sfh <- data.frame(abrechnungsjahr = sfh$abrechnungsjahr, bundesland = sfh$bundesland, 
                    mean = sfh$mean)[abrechnungsjahr == year,]

  
  all_area_m <- sfh_area
  names(all_area_m) <- c('abrechnungsjahr', states)
  names(mfh_area) <- c('abrechnungsjahr', states)
  for (state in states) {
    all_area_m[[state]] <- as.numeric(as.character(all_area_m[[state]])) + as.numeric(as.character(mfh_area[[state]]))
  }
  all <- convert_area_to_co2_emissions(all_area_m, rbind(DL_MFH, DL_SFH), co2_coef)
  all$mean <- apply(all[enegietraeger], 1, sum)
  all <- data.frame(abrechnungsjahr = all$abrechnungsjahr, bundesland = all$bundesland, 
                    mean = all$mean)[abrechnungsjahr == year,]
  
  
  data <-data.frame(bundesland = unique(DL_MFH$bundesland), MFH = rep(NA, 16), SFH = rep(NA, 16), ALL = rep(NA, 16))
  
  for (state in states) {
    data[data$bundesland == state,]$MFH <- mfh[mfh$bundesland == state,]$mean / 10^2
    data[data$bundesland == state,]$SFH <- sfh[sfh$bundesland == state,]$mean / 10^2
    data[data$bundesland == state,]$ALL <- all[all$bundesland == state,]$mean / 10^2
  }
  data
<<<<<<< HEAD
}
=======
}

get_map_data_co2()
>>>>>>> a720390a950e1cf14be44f8f88afec3387a3e506
