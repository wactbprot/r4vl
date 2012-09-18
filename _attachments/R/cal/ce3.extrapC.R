ce3.extrapC <- function(ccc){
  msg <- "calculated by ce3.extrapC"

  a     <-  abbrevList(ccc)
  
  ##      return(list(iLw1 = iLw1,   ## grosser LW
  ##                  iLw2 = iLw2))  ## kleiner LW
  ##  
  ilw   <-  getConductIndex(ccc)
  
  if(a$cmscg == "N2"){
    if(length(ilw$iLw2) > 0){
      ##
      ## N2 kleiner LW
      ##
    }
    if(length(ilw$iLw1) > 0){
      ##
      ## N2 großer LW
      ##
    }

  }

  
  
  ccc$Calibration$Analysis$Values$Conductance <-
    setCcl(ccc$Calibration$Analysis$Values$Conductance,
           "cfm3",
           "l/s",
           L,
           msg)
  
  return(ccc)
}
