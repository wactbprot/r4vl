ce3.uncertCmol <- function(ccc){

  msg <- "Calculated by ce3.uncerCmol()"
  a   <- abbrevList(ccc)

  
  uDVList   <- getSubList(a$cav, "uncertDeltaV")
  uDtList   <- getSubList(a$cav, "uncertDeltat")
  uDVDtList <- getSubList(a$cav, "uncertDeltaVDeltat")
  uFmol     <- getSubList(a$cav, "uncertFmol")

  ## relative Unsicherheitsbeiträge werden quadr. addiert
  if((uDVList$Unit  ==  uDtList$Unit)   &
     (uDVList$Unit  ==  uDVDtList$Unit) &
     (uDVList$Unit  ==  uFmol$Unit)     &
     (uDVList$Unit  ==  "1")){
    
    uCmol   <- sqrt(uDVList$Value^2 + uDtList$Value^2 +  uDVDtList$Value^2 + uFmol$Value^2)
    resUnit <- "1"

  }else{
    
    msg     <- paste(msg,
                     " Units do not match ",
                     " (uncertDeltaV,uncertDeltat, uncertDeltaVDeltat, uncertFmol):",
                     uDVList$Unit,
                     uDtList$Unit,
                     uDVDtList$Unit,
                     uFmol$Unit)
    
  }
  
  ccc$Calibration$Analysis$Values$Uncertainty <-
    setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
           "uncertCmol",
           resUnit,
           uCmol,
           msg)
  
  return(ccc)
}
