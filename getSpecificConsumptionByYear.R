getAreaConsumptionByYear <- function(obj) {
  require(dplyr)
  by_year <- group_by(obj,abrechnungsjahr)
  return_data <- as.data.frame(summarize(by_year,sum(gebaeude_nutzflaeche),sum(verbrauch_gesamt_kwh)))
  names(return_data) <- c("abrechnungsjahr","Area","Consumption")
  detach(package:dplyr)
  return(return_data)
}

getSpecificConsumptionByYear <- function(mfh,sfh,gtype) {#gtype: MFH, SFH, or ALL
  if (gtype=="MFH") {
    MFH_byyear <- getAreaConsumptionByYear(mfh)
    MFH_byyear$spz_verbrauch <- MFH_byyear$Consumption / MFH_byyear$Area
    return_data <- MFH_byyear
  }
  if (gtype == "SFH") {
    SFH_byyear <- getAreaConsumptionByYear(sfh) 
    SFH_byyear$spz_verbrauch <- SFH_byyear$Consumption / SFH_byyear$Area
    return_data <- SFH_byyear
  }
  if (gtype == "ALL") {
    MFH_byyear <- getAreaConsumptionByYear(mfh)
    SFH_byyear <- getAreaConsumptionByYear(sfh)
    ALL_byyear <- MFH_byyear + SFH_byyear
    ALL_byyear$abrechnungsjahr <- 2002:2018
    ALL_byyear$spz_verbrauch <- ALL_byyear$Consumption / ALL_byyear$Area
    return_data <- ALL_byyear
  }
  return(return_data)
}

qf(0.25,2,188)
