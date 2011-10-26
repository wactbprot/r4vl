uncertCmol <- function(ccc){

  msg <- "Calculated by uncerCmol()"

  tmpAn     <- ccc$Calibration$Analysis
  tmpUns    <- tmpAn$Values$Uncertainty

  uDVList   <- getSubList(tmpUns, "uncertDeltaV")
  uDtList   <- getSubList(tmpUns, "uncertDeltat")
  uDVDtList <- getSubList(tmpUns, "uncertDeltaVDeltat")
  uFmol     <- getSubList(tmpUns, "uncertFmol")

  ## relative Unsicherheitsbeiträge werden quadr. addiert
  if((uDVList$Unit ==  uDtList$Unit)  &
     (uDVList$Unit == uDVDtList$Unit) &
     (uDVList$Unit  == uFmol$Unit)    &
     (uDVList$Unit  =="1")){

    uCmol =sqrt(uDVList$Value^2 + uDtList$Value^2 +  uDVDtList$Value^2 + uFmol$Value^2)
    resUnit <- "1"
  }else{
    print("Units do not match (uncertDeltaV,uncertDeltat, uncertDeltaVDeltat, uncertFmol):",
          uDVList$Unit,  uDtList$Unit,uDVDtList$Unit,uFmol$Unit)
      stop()
  }

  ccc$Calibration$Analysis$Values$Uncertainty <- setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
                                                          "uncertCmol",
                                                          resUnit,
                                                          uCmol,
                                                          msg)
  return(ccc)
}
