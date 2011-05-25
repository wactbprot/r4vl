clearCustomCalOb <- function(cdb,ccc){
  ## laut Vereinbarung ist das erste CO immer das
  ## Kundengerät !

  tempCalibrationObjects <- ccc$Calibration$Measurement$CalibrationObject

  noOfAECalibrationObjects <- length(tempCalibrationObjects)

  simpleCo <-  list( ##
                   Comment="place holder for cusomer CalibrationObject",
                   Owner="nobody"
                 )
  ##
  ## ersetzt bisher nur eine in CO vorcommende id durch das entsprechende Object.
  ## reicht Vieleicht auch
  ##
  if(noOfAECalibrationObjects == 1 ){
    ## dann wahrscheinlich id Eintrag:
    if(length(tempCalibrationObjects$id) == 1){
      ## dann ersetze die id durch das CalibrationObjectDoc

      cdb$id <- tempCalibrationObjects$id

      CAresponse <-   cdbGetDoc(cdb)$res

      if(length(CAresponse$CalibrationObject) > 0){
        ## dann reinbasteln
        tempCalibrationObjects[[1]] <- CAresponse$CalibrationObject


      }else{
        tempCalibrationObjects[[1]] <- simpleCo
      }
    }
  }else{
    ## es gibt noch kein CalibrationObject
    ## oder es gibt mehr als eins
    if(noOfAECalibrationObjects == 0  ){# es gibt noch kein CO
      tempCalibrationObjects[[1]] <- simpleCo
    }

  }


  ##  cdb$id <- "608b6039cfad861bb4ee96e176d8ade6"  ##R30
  ##  cdb$id <- "cb70fddb59e39b147d219f84ff12efa8"  ##R27
  ##  cdb$id <- "fc8e73d564e0f737428fe8ea46032e6e" ## BA16
  ##  cdb$id <- "fc8e73d564e0f737428fe8ea46061ecd" ## SI404
  ## ccc$Calibration$Measurement$CalibrationObject[[1]] <-   cdbGetDoc(cdb)$res$CalibrationObject #tempCalibrationObjects[[1]]
  ccc$Calibration$Measurement$CalibrationObject[[1]] <- simpleCo

    return(ccc)
}
