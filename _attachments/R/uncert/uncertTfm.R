uncertTfm <- function(ccc){

  msg <- "calculated by uncertTfm()"

  tmpAn     <- ccc$Calibration$Analysis
  tmpMea    <- ccc$Calibration$Measurement
  tmpStrd   <- tmpMea$Standard
  tmpCo     <- tmpMea$CalibrationObject

  TfmList <- getSubList(tmpAn, "Tfm3")

  noOfCo    <- length(tmpCo)
  noOfTfm <- length(TfmList$Value)

  uncertTfmRes <- rep(NA,noOfTfm)

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
      if(length(currCo$Device$UsedFor) > 0){
        if(currCo$Device$UsedFor == "T"){

          res <- quadrSumContrib(currCo, TfmList,uncertTfmRes,msg)
          uncertTfmRes <- res$uncertRes
          msg <- res$msg
        }
      }
    }
    ## gibt es NA's in uncertTfmRes?
    iall <- which(is.na(uncertTfmRes))

    if(length(iall) > 0){
      print("uncertainty vector don't cover entire Tfm range")
      print("using u=1 (100%) instead!")
      msg <- paste(msg, "replaced uncalculated value at point(s)",iall,"with 1 (100%)")
      uncertTfmRes[iall] <- 1.0

    }
    ccc$Calibration$Analysis$Values$Uncertainty <- setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
                                                          "uncertTfm",
                                                          "1",
                                                          uncertTfmRes,
                                                          msg)
  }## no ofCo<2

  return(ccc)
}

