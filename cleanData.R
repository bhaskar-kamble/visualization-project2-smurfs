cleanData <- function(obj,gtype) {
  #spz_verbrauch_low <- 
  # relevant features
  # 1. verbrauch_gesamt_kwh
  # 2. gebaeude_nutzflaeche
  # 3. gebaeude_baujahr
  # 4. verbrauch_gesamt_kwh_spez
  
  check_cols <- c("verbrauch_gesamt_kwh",
                  "gebaeude_nutzflaeche",
                  "gebaeude_baujahr")
  MFH_low  <- c(  5000 ,  200 , 1100)
  MFH_high <- c(700000 , 2400 , 2018)
  SFH_low  <- c(  1000 ,   60 , 1100)
  SFH_high <- c( 70000 , 2000 , 2018)
  
  if (gtype=="MFH") {
    limits_low <- MFH_low
    limits_high <- MFH_high
  }
  if (gtype=="SFH") {
    limits_low <- SFH_low
    limits_high <- SFH_high
  }
  
  #MFH_verbrauch_gesamt_kwh_low <- 5000
  #MFH_verbrauch_gesamt_kwh_high <- 700000
  #SFH_verbrauch_gesamt_kwh_low <- 1000
  #SFH_verbrauch_gesamt_kwh_high <- 70000

  #MFH_gebaeude_nutzflaeche_low <- 200.0 
  #MFH_gebaeude_nutzflaeche_high <- 2400.0 
  #SFH_gebaeude_nutzflaeche_low <- 60.0 
  #SFH_gebaeude_nutzflaeche_high <- 2000.0
  

  #MFH_gebaeude_baujahr_low  <- 1100
  #MFH_gebaeude_baujahr_high <- 2018
  #SFH_gebaeude_baujahr_low  <- 1100
  #SFH_gebaeude_baujahr_high <- 2018
  
  for (i in 1:length(check_cols)) {
    current_var <- check_cols[i]
    is_in_range <- (obj[[current_var]] > limits_low[i]) & (obj[[current_var]] < limits_high[i])
    obj <- obj[is_in_range , ]
  }

  return(obj)
  
}