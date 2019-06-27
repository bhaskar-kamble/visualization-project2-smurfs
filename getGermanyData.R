getGermanyData <- function(gtype) { #gtype can be either SFH or MFH
  
  path_to_file <- "~/Desktop/visualization-project2-smurfs"
  
  if (gtype == "SFH") {
    load( paste0(path_to_file , "/", "SFH20022018_v2.RData"))
    
    SFH20022018$abrechnungsjahr <- as.integer(SFH20022018$abrechnungsjahr)
    SFH20022018$verbrauch_gesamt_kwh <- gsub("," , "." , SFH20022018$verbrauch_gesamt_kwh)
    SFH20022018$verbrauch_gesamt_kwh <- as.numeric(SFH20022018$verbrauch_gesamt_kwh)
    SFH20022018$gtype <- "SFH"
    
    SFH20022018$energietraeger[SFH20022018$energietraeger=="fernwaerme"] <- "waerme"
    SFH20022018$energietraeger[SFH20022018$energietraeger=="nahwaerme"] <- "waerme"
    SFH20022018$energietraeger[SFH20022018$energietraeger=="waermepumpe"] <- "strom"
    SFH20022018$energietraeger[SFH20022018$energietraeger=="waermepumpenstrom"] <- "strom"
    SFH20022018$energietraeger[SFH20022018$energietraeger=="waermepumpe_luft"] <- "strom"
    SFH20022018$energietraeger[SFH20022018$energietraeger=="waermepumpe_wasser"] <- "strom"
    
    SFH20022018$sto_plz <- as.integer(SFH20022018$sto_plz)
    return_data <- SFH20022018
  }
  
  if (gtype == "MFH") {
    load( paste0(path_to_file , "/", "MFH20022018_v2.RData"))
    MFH20022018$gtype <- "MFH"
    
    MFH20022018 <- MFH20022018[MFH20022018$energietraeger != "braunkohle" , ]
    MFH20022018$energietraeger[MFH20022018$energietraeger=="fernwaerme"] <- "waerme"
    MFH20022018$energietraeger[MFH20022018$energietraeger=="nahwaerme"] <- "waerme"
    MFH20022018$energietraeger[MFH20022018$energietraeger=="waermepumpe"] <- "strom"
    MFH20022018$energietraeger[MFH20022018$energietraeger=="waermepumpenstrom"] <- "strom"
    MFH20022018$energietraeger[MFH20022018$energietraeger=="waermepumpe_luft"] <- "strom"
    MFH20022018$energietraeger[MFH20022018$energietraeger=="waermepumpe_wasser"] <- "strom"
    
    MFH20022018$sto_plz <- as.integer(MFH20022018$sto_plz)
    return_data <- MFH20022018
  }
  
  return(return_data)
}