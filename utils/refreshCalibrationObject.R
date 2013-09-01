refreshCalibrationObject <- function(cdb,ccc){
  ## temp Calibraion Object
  ## das Co wird neu zusammengebaut
  tCo <- list()
  tCo[[1]] <- ccc$Calibration$Measurement$CalibrationObject[[1]]

  noOfCo <- 1
  currCo <- 1

  cdb$design <- "standard_ce3"

  cdb$view <- "get_calibrations"
  relCal <- cdbGetView(cdb)$res

  cdb$view <- "get_calibrationobjects"
  relCalObs <- cdbGetView(cdb)$res

  ## vector mit allen related CalibrationObjectSigns
  coSignVec  <- unlist(lapply(relCalObs$rows, function(i){return(i$value$Sign)}))
  ## vector mit allen related CalibrationSigns
  calSignVec <- unlist(lapply(relCal$rows, function(i){return(i$value$Sign)}))

  ## Hier steckt die muß-Annahme drin CO -kann-> CA
  ## und
  ## eine IK -muß-> CO
  ## haben
  while(currCo <= length(coSignVec)){

    currCo <- giveLastPos(coSignVec,coSignVec[currCo])

    tCo[[noOfCo+1]] <- relCalObs$rows[[currCo]]$value$Values

    currCal <- giveLastPos(calSignVec,coSignVec[currCo])

    if(currCal > 0){
      tCo[[noOfCo + 1 ]]$Values <-
        relCal$rows[[currCal]]$value$Values
    }

    currCo <- currCo + 1
    noOfCo <- noOfCo + 1
  }

  ccc$Calibration$Measurement$CalibrationObject   <- tCo

  return(ccc)
}
