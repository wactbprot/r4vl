uncertqpV <- function(ccc){

  msg <- "Calculated by uncertqpV()"

  tmpAn     <- ccc$Calibration$Analysis
  tmpUns    <- tmpAn$Values$Uncertainty

  uPfillList <- getSubList(tmpUns, "uncertPfill")
  uDVList    <- getSubList(tmpUns, "uncertDeltaV")
  uDtList    <- getSubList(tmpUns, "uncertDeltat")
  uDVDtList  <- getSubList(tmpUns, "uncertDeltaVDeltat")

  if((uPfillList$Unit  ==  uDVList$Unit)  &
     (uPfillList$Unit  ==  uDVDtList$Unit) &
     (uPfillList$Unit  ==  uDtList$Unit)    &
     (uPfillList$Unit  ==  "1")){

    uncertRes <- sqrt(uPfillList$Value^2 +  uDVList$Value^2 + uDtList$Value^2 + uDVDtList$Value^2)

    ccc$Calibration$Analysis$Values$Uncertainty <- setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
                                                          "uncertqpV",
                                                          "1",
                                                          uncertRes,
                                                          msg)
  }

  return(ccc)

}
