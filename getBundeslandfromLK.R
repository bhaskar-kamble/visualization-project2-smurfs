getBundeslandfromLK <- function(obj) {
  states <- unique(obj$bundesland)
  states <- states[states!="unbekannt"]
  for (s in states) {
    obj$bundesland[
      obj$bundesland=="unbekannt" & 
        obj$Landkreis_von_GS %in% obj$Landkreis_von_GS[obj$bundesland==s]
      ] <- s
  }
  #return(landkreis_Bland)
  return(obj)
}