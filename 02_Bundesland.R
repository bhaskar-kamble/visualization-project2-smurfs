path_to_file <- "D:/GITHUB_REPOS/visualization-project2-smurfs"
source(paste0(path_to_file , "/" , "getGermanyData.R"))
source(paste0(path_to_file , "/" , "cleanData.R"))
source(paste0(path_to_file , "/" , "getBundeslandfromCode.R"))
source(paste0(path_to_file , "/" , "getBundeslandfromLK.R"))

#Get the yearly development of specific consumption

DL_MFH <- getGermanyData(gtype = "MFH")
DL_SFH <- getGermanyData(gtype = "SFH")
#gtype: MFH or SFH

#boxplot(DL_SFH$gebaeude_baujahr)

DL_MFH <- cleanData(DL_MFH , "MFH")
DL_SFH <- cleanData(DL_SFH , "SFH")

DL_MFH <- getBundeslandfromCode(DL_MFH)
DL_SFH <- getBundeslandfromCode(DL_SFH)

#(1)
#> sum(is.na(DL_SFH$bundesland))
#[1] 958    - THESE HAVE EVERYTHING NA; INCLUDING LANDKREIS_VON_GS; SO LATTER CANNOT BE USED TO DETERMINE BUNDESLAND
#(2)
#> sum(DL_SFH$bundesland=="unbekannt",na.rm=TRUE)
#[1] 748
#(3)
#> dim(DL_SFH[DL_SFH$bundesland=="unbekannt" , ])
#[1] 1706   10
# There is a problem in the above. (1) says nnumber of NAs is 958. (2) says number of unbekannt is 748. But (3)
#says that the number of unbekannt is 1706 (i.e. = 958+748). How can this be?
#Here is the explanation:
#> xtest <- c("a","b",NA,"c","d",NA,"e")
#> xtest[xtest=="d"]
#[1] NA  "d" NA 
#so first remove all the bundesland with NA (these have also Landkreis_von_GS as NA)
DL_SFH <- DL_SFH[!is.na(DL_SFH$bundesland) , ]
DL_SFH <- getBundeslandfromLK(DL_SFH)

#now subset bundesland and do the calculations for specific consumption
# (1) MFH case
states <- unique(DL_MFH$bundesland)
for (s in states) {
  #calculate the specific consumption and the slope
  
}