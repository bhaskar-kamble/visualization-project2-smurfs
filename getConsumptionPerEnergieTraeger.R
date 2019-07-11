relative_freq <- function(data) {
  data_t <- data[,c('abrechnungsjahr','energietraeger', 'verbrauch_gesamt_kwh')]
  T <- aggregate(data_t[c(-1, -2)], by=list(data_t$abrechnungsjahr, data_t$energietraeger), FUN=length)
  data_aggregated = T[c('Group.1', 'Group.2', 'verbrauch_gesamt_kwh')]
  colnames(data_aggregated) <- c('abrechnungsjahr', 'energietraeger', 'value')
  
  data_t <- data[,c('abrechnungsjahr', 'verbrauch_gesamt_kwh')]
  by_year <- aggregate(data_t[c(-1)], by=list(data$abrechnungsjahr), FUN=length)
  by_year <- by_year[]
  
  for (year in c(2002:2018)) {
    data_aggregated[data_aggregated$abrechnungsjahr == year, ]$value <- 
      data_aggregated[data_aggregated$abrechnungsjahr == year, ]$value / by_year[by_year$Group.1 == year, ]$verbrauch_gesamt_kwh
  }
  return(data_aggregated)
}


get_visibility_combinations <- function() {
  visibility_combinations <- list()
  i = 1
  for (state in states) {
    to = 16*6 + 1
    vis <- rep(F, to)
    for(k in ((i - 1) * 6 + 2): (i * 6 + 1))
      vis[k] <- TRUE
    visibility_combinations[[i]] <- list(method = "restyle",
                                                        args = list("visible", as.list(vis)),
                                                        label = state)
    i = i + 1
  }
  return(visibility_combinations)
}

prepare_data <- function(data) {
  data <- data[,c('abrechnungsjahr', 'energietraeger','bundesland','verbrauch_gesamt_kwh')]
  T <- aggregate(data[c(-1,-2,-3)], by=list(data$abrechnungsjahr, data$energietraeger, data$bundesland), FUN=length)
  data_aggregated = T[c('Group.1', 'Group.2', 'Group.3' ,'verbrauch_gesamt_kwh')]
  colnames(data_aggregated) <- c('abrechnungsjahr', 'energietraeger',  'bundesland', 'value')
  return(data_aggregated)
}


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