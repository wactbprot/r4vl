ce3.uncertDeltaVDeltat <- function(ccc){
  msg <- "Calculated by ce3.uncertDeltaVDeltat()"

  tmpAn     <- ccc$Calibration$Analysis
  tmpMea    <- ccc$Calibration$Measurement
  tmpStrd   <- tmpMea$Standard
  tmpCo     <- tmpMea$CalibrationObject

  pfillList <- getSubList(tmpAn, "fill")
  pfill     <- pfillList$Value
  pfillUnit <- pfillList$Unit

  uncertRes <- rep(1,length(pfill))

  iLw1 <-  getConductIndex(ccc)$iLw1
  iLw2 <-  getConductIndex(ccc)$iLw2

  if(length(iLw1) > 0){

    u1List <- getSubList(tmpStrd$Values,"fm3DeltaVDeltatLw1_u1")
    iu1 <- checkUncertRange(u1List, pfillList, iLw1)
    msg <- paste(msg, "points: ", toString(iu1), "belong to Lw1, use: ", u1List$Type)
    uncertRes[iu1] <- as.double(u1List$Value)

  }

  if(length(iLw2) > 0){

    u1aList <- getSubList(tmpStrd$Values,"fm3DeltaVDeltatLw2_u1_a")
    iu1a <- checkUncertRange(u1aList, pfillList, iLw2)

    u1bList <- getSubList(tmpStrd$Values,"fm3DeltaVDeltatLw2_u1_b")
    iu1b <- checkUncertRange(u1bList, pfillList, iLw2)

    u1cList <- getSubList(tmpStrd$Values,"fm3DeltaVDeltatLw2_u1_c")
    iu1c <- checkUncertRange(u1cList, pfillList, iLw2)

    ## es geht hier (nachfolg. if statement darum die derzeitige impl.
    ## der beiden Unsicherh. sicherzustellen
    if(length(iu1a) == length(iu1b)){
      uncertRes[iu1b] <- as.double(u1aList$Value) + log(1/pfill[iu1b]) * as.double( u1bList$Value)

      msg <- paste(msg, "points: ", toString(iu1b), "belong to Lw2, use: ",
                   u1aList$Type, " and ", u1bList$Type)
    }

    if(length(iu1c) > 0){
      uncertRes[iu1c] <- as.double(u1cList$Value)

      msg <- paste(msg, "points: ", toString(iu1c), "belong to Lw2, use: ",
                   u1cList$Type)
    }
  }

  ccc$Calibration$Analysis$Values$Uncertainty <- setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
                                                        "uncertDeltaVDeltat",
                                                        "1",
                                                        uncertRes,
                                                        msg)


  return(ccc)
}
