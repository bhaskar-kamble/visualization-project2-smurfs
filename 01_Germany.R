path_to_file <- "D:/GITHUB_REPOS/visualization-project2-smurfs"
source(paste0(path_to_file , "/" , "getGermanyData.R"))
source(paste0(path_to_file , "/" , "cleanData.R"))

#Get the yearly development of specific consumption

DL_MFH <- getGermanyData(gtype = "MFH")
DL_SFH <- getGermanyData(gtype = "SFH")
#gtype: MFH or SFH

#boxplot(DL_SFH$gebaeude_baujahr)

DL_MFH <- cleanData(DL_MFH , "MFH")
DL_SFH <- cleanData(DL_SFH , "SFH")

# After cleaning:
#> nrow(DL_SFH)
#[1] 1829935
#> nrow(DL_MFH)
#[1] 365009
#> nrow(DL_MFH) + nrow(DL_SFH)
#[1] 2194944

# Before cleaning:
#> nrow(DL_SFH)
#[1] 1862426
#> nrow(DL_MFH)
#[1] 423489
#> nrow(DL_MFH) + nrow(DL_SFH)
#[1] 2285915

#MFH:
#> 365009/423489
#[1] 0.861909
#SFH:
#> 1829935/1862426
#[1] 0.9825545

# Find the Total Area for each year
# Find the Total energy consumption for each year
# For each year, divide the energy consumption by the area to get the specific consumption

# DIVIDE AREA BY 1.2 !!!!!!

# TASK 1 : - Get the specific consumption of Germany for MFH, SFH and ALL buildings-----------------------

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
#MFH_byyear <- getAreaConsumptionByYear(DL_MFH)
#SFH_byyear <- getAreaConsumptionByYear(DL_SFH)
#both_byyear <- 
MFH_spez_verbrauch <- getSpecificConsumptionByYear( DL_MFH , DL_SFH , gtype = "MFH")
SFH_spez_verbrauch <- getSpecificConsumptionByYear( DL_MFH , DL_SFH , gtype = "SFH")
ALL_spez_verbrauch <- getSpecificConsumptionByYear( DL_MFH , DL_SFH , gtype = "ALL")

# Now plot:
require(ggplot2)
g_mfh <- ggplot() + geom_point(data=MFH_spez_verbrauch , aes(x=abrechnungsjahr,y=spz_verbrauch)
)+scale_y_continuous(limits=c(0,150))

g_sfh <- ggplot() + geom_point(data=SFH_spez_verbrauch , aes(x=abrechnungsjahr,y=spz_verbrauch)
)+scale_y_continuous(limits=c(0,150))

g_all <- ggplot() + geom_point(data=ALL_spez_verbrauch , aes(x=abrechnungsjahr,y=spz_verbrauch)
)+scale_y_continuous(limits=c(0,150))

#point to note: 2010 was very cold. https://wiki.bildungsserver.de/klimawandel/index.php/Kalte_Winter_in_Europa
# https://wetterkanal.kachelmannwetter.com/dezember-2010-ein-land-versinkt-im-schnee/
# https://www.wetteronline.de/wetterrueckblick/rueckblick-dezember-2010-sehr-kalt-und-extrem-schneereich-2010-12-30-rd
# http://www.frontgewitter.de/Winter1011.html

# https://de.wikipedia.org/wiki/Zeitreihe_der_Lufttemperatur_in_Deutschland#/media/Datei:Zeitreihe_der_Temperaturen_in_Deutschland.svg
# https://de.wikipedia.org/wiki/Zeitreihe_der_Lufttemperatur_in_Deutschland


# Task 1 done ---------------------------------------------------------------------------

# Task 2: you can find the linear trend of the energy consumption for the different Landkreise and compare them.
# Maybe show them on a map. OR for the Bundesländer.

# Task 3: Get the shares of the different Energietraegers - what proportion of energy each ET contributes.
# Again a heat map for the Bundesländer or Landkreise.

# BY THE WAY - the number of data points for each year must also be shown


# leaflet package for interactive maps
#tmap - uses leaflet