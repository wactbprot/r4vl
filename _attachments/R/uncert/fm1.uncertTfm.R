fm1.uncertTfm <- function(ccc){

  msg <- "calculated by fm1.uncertTfm()"

  a <- abbrevList(ccc)

  TFM       <- getSubList(a$cav$Temperature, "fm1")
  Tfm       <- getConstVal(NA,NA,TFM)
  TfmUnit   <- TFM$Unit 
  
  noOfCo    <- length( a$cmco)
  noOfTfm   <- length(Tfm)
  
  uncertTSensCalib <- rep(NA,noOfTfm)
  
  if( noOfCo < 2){
    print("No. of CalibrationObjects < 2")
    stop()
  }else{
    
    for(ico in 1:noOfCo){
      currCo <- a$cmco[[ico]]
      ## Vorsicht es werden alle Calibration Objekte angesehen!!
      ## deshalb noch UsedFor Tag eingef.
      ## ok!
      ## function abstrahiert mit quadrSumContrib()
      if(length(currCo$Device$UsedFor) > 0){
        if(currCo$Device$UsedFor == "T"){

          res <- quadrSumContrib(currCo, TFM,uncertTSensCalib,msg)
          uncertTSensCalib <- res$uncertRes
          msg <- res$msg
        }
      }
    }
    ## gibt es NA's in uncertTSensCalib?
    iall <- which(is.na(uncertTSensCalib))

    if(length(iall) > 0){
      msg <- paste(msg,
                   "uncertainty vector don't cover entire Tfm range ",
                   "use u=1 (100%) instead! ",
                   "replaced uncalculated value at ",length(iall)," point(s): ",
                    )

      uncertTSensCalib[iall] <- 1.0

    }
    
    UNS1 <- getSubList(a$cms, "fm1Tfm_u1")
    UNS2 <- getSubList(a$cms, "fm1Tfm_u2")
    UNS3 <- getSubList(a$cms, "fm1Tfm_u3")

    if(TfmUnit == UNS1$Unit &&
       TfmUnit == UNS2$Unit &&
       TfmUnit == UNS3$Unit){

      u1 <- getConstVal(NA,NA, UNS1)
      u2 <- getConstVal(NA,NA, UNS2)
      u3 <- getConstVal(NA,NA, UNS3)
    
    uges <- sqrt((uncertTSensCalib*Tfm)^2 + u1^2 + u2^2 + u3^2)/Tfm

  }else{
    
    uges <- 2/Tfm ## muss weh tun!
    
    msg <- paste(msg, " uncert. contrib. units dont match! use 2K as uncertainty")
  }
    ccc$Calibration$Analysis$Values$Uncertainty <-
      setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
             "uncertTfm",
             "1",
              uges,
             msg)
  }## no ofCo<2

  return(ccc)
}

