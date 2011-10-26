uncertPfill <- function(ccc){

  msg <- "Calculated by uncertPfill()"

  tmpAn     <- ccc$Calibration$Analysis
  tmpMea    <- ccc$Calibration$Measurement
  tmpStrd   <- tmpMea$Standard
  tmpCo     <- tmpMea$CalibrationObject

  pfillList <- getSubList(tmpAn, "fill")

  noOfCo    <- length(tmpCo)
  noOfPfill <- length(pfillList$Value)

  uncertPfillRes <- rep(NA,noOfPfill)

  if( noOfCo < 2){
    print("No. of CalibrationObjects < 2")
    stop()
  }else{

    for(ico in 1:noOfCo){
      currCo <- tmpCo[[ico]]
      ## Vorsicht es werden alle Calibration Objekte angesehen!!
      ## deshalb noch UsedFor Tag eingef.
      ## ok!
      ## function abstrahiert mit quadrSumContrib()
      ## die Tatsache, dass man die function calVarianz()
      ## aufrufen kann und nur pfill bezogene Anteile berücksichtigt,
      ## liegt an der if Abfrage des des usedFor Tags
      if(length(currCo$Device$UsedFor) > 0){
        if(currCo$Device$UsedFor == "pfill"){
          ## der uncertPfillRes param muß übergeben werden, weil
          ## unterschiedliche CalibrationObjects unterschiedliche Bereich
          ## des (hier) pfill ausfüllen
          res <- quadrSumContrib(currCo, pfillList,uncertPfillRes,msg)
          uncertPfillRes <- res$uncertRes
          msg <- res$msg
        }
      }
    }
    ## gibt es NA's in uncertPfillRes?
    iall <- which(is.na(uncertPfillRes))

    if(length(iall) > 0){
      print("uncertainty vector don't cover entire pfill range")
      print("using u=1 (100%) instead!")
      msg <- paste(msg, "replaced uncalculated value at point(s)",iall,"with 1 (100%)")
      uncertPfillRes[iall] <- 1.0

    }
    ccc$Calibration$Analysis$Values$Uncertainty <- setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
                                                          "uncertPfill",
                                                          "1",
                                                          uncertPfillRes,
                                                          msg)
  }## no ofCo<2
  return(ccc)
}
