absolute_freq <- function(data) {
  data <- data[,c('abrechnungsjahr','energietraeger', 'verbrauch_gesamt_kwh')]
  T <- aggregate(data[c(-1, -2)], by=list(data$abrechnungsjahr, data$energietraeger), FUN=sum)
  data_aggregated = T[c('Group.1', 'Group.2', 'verbrauch_gesamt_kwh')]
  colnames(data_aggregated) <- c('abrechnungsjahr', 'energietraeger', 'value')
  return(data_aggregated)
}

relative_freq <- function(data) {
  data_t <- data[,c('abrechnungsjahr','energietraeger', 'verbrauch_gesamt_kwh')]
  T <- aggregate(data_t[c(-1, -2)], by=list(data_t$abrechnungsjahr, data_t$energietraeger), FUN=sum)
  data_aggregated = T[c('Group.1', 'Group.2', 'verbrauch_gesamt_kwh')]
  colnames(data_aggregated) <- c('abrechnungsjahr', 'energietraeger', 'value')
  
  data_t <- data[,c('abrechnungsjahr', 'verbrauch_gesamt_kwh')]
  by_year <- aggregate(data_t[c(-1)], by=list(data$abrechnungsjahr), FUN=sum)
  by_year <- by_year[]
  
  for (year in c(2002:2018)) {
    data_aggregated[data_aggregated$abrechnungsjahr == year, ]$value <- 
      data_aggregated[data_aggregated$abrechnungsjahr == year, ]$value / by_year[by_year$Group.1 == year, ]$verbrauch_gesamt_kwh
  }
  return(data_aggregated)
}



absolute_freq_interactive <- function(data) {
  data_aggregated <- prepare_data(data)
  data_aggregated <- as.data.frame(complete(data_aggregated, abrechnungsjahr, energietraeger, bundesland, fill = list(value = 0)))
  data_aggregated[is.na(data_aggregated)] <- 0
  data_wide <- spread(data_aggregated, energietraeger, value)
  return(as.data.frame(data_wide))
}


relative_freq_interactive <- function(data) {
  data_aggregated <- prepare_data(data)
  
  data_d <- data[c('abrechnungsjahr', 'bundesland', 'verbrauch_gesamt_kwh')]
  by_year <- aggregate(data_d[c(-1, -2)], by=list(data_d$abrechnungsjahr, data_d$bundesland), FUN=sum)
  
  data_aggregated <- as.data.frame(complete(data_aggregated, abrechnungsjahr, energietraeger, bundesland, fill = list(value = 0)))
  
  for (year in c(2002:2018)) {
    for (state in states) {
      data_aggregated[data_aggregated$abrechnungsjahr == year & data_aggregated$bundesland == state, ]$value <- 
        data_aggregated[data_aggregated$abrechnungsjahr == year & data_aggregated$bundesland == state, ]$value / by_year[by_year$Group.1 == year & by_year$Group.2 == state, ]$verbrauch_gesamt_kwh
    }
  }
  data_aggregated[is.na(data_aggregated)] <- 0
  data_wide <- spread(data_aggregated, energietraeger, value)
  data_wide
  
  return(as.data.frame(data_wide))
}


get_visibility_combinations <- function() {
  visibility_combinations <- list()
  i = 2
  visibility_index = 1
  for (state in states) {
    to = 16*6 + 1
    vis <- rep(F, to)
    for(k in ((i - 1) * 6 + 2): (i * 6 + 1))
      vis[k] <- TRUE
    visibility_combinations[[visibility_index]] <- list(method = "restyle",
                                                        args = list("visible", as.list(vis)),
                                                        label = state)
    i = i + 1
    visibility_index = visibility_index + 1
  }
  return(visibility_combinations)
}

prepare_data <- function(data) {
  data <- data[,c('abrechnungsjahr', 'energietraeger','bundesland','verbrauch_gesamt_kwh')]
  T <- aggregate(data[c(-1,-2,-3)], by=list(data$abrechnungsjahr, data$energietraeger, data$bundesland), FUN=sum)
  data_aggregated = T[c('Group.1', 'Group.2', 'Group.3' ,'verbrauch_gesamt_kwh')]
  colnames(data_aggregated) <- c('abrechnungsjahr', 'energietraeger',  'bundesland', 'value')
  return(data_aggregated)
}
