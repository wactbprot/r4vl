ce3.uncertTch <- function(ccc){

  msg <- "calculated by ce3.uncertTch()"
  
  a <- abbrevList(ccc)
  
  TUHV        <- getSubList(a$cav, "Tuhv")
  Tch         <- getConstVal(NA,NA,TUHV)
  TchUnit     <- TUHV$Unit 
  
  noOfCo      <- length( a$cmco)
  noOfTch     <- length(Tch)
  
  uncertTSensCalib <- rep(NA,noOfTch)
  
  if( noOfCo < 2){
    stop("No. of CalibrationObjects < 2")
  }else{
    
    for(ico in 1:noOfCo){
      currCo  <- a$cmco[[ico]]
      ## Vorsicht es werden alle Calibration Objekte angesehen!!
      ## deshalb noch UsedFor Tag eingef.
      ## ok!
      ## function abstrahiert mit quadrSumContrib()
      
      if(length(currCo$Device$UsedFor) > 0){
        if(currCo$Device$UsedFor == "T"){

          res              <- quadrSumContrib(currCo, TUHV,uncertTSensCalib,msg)
          uncertTSensCalib <- res$uncertRes
          msg              <- res$msg
        }
      }
    }
    ## gibt es NA's in uncertTSensCalib?
    iall    <- which(is.na(uncertTSensCalib))
    
    if(length(iall) > 0){
      msg   <- paste(msg,
                   "uncertainty vector don't cover entire Tch range ",
                   "use u=1 (100%) instead! ",
                   "replaced uncalculated value at point(s): ",
                   toString(iall))
      
      uncertTSensCalib[iall] <- 1.0
      
    }
    
    UNS1   <- getSubList(a$cms, "ce3Tch_u1")
    UNS2   <- getSubList(a$cms, "ce3Tch_u2")
    
    if(TchUnit == UNS1$Unit & TchUnit == UNS2$Unit ){
      
      u1   <- getConstVal(NA,NA, UNS1)
      u2   <- getConstVal(NA,NA, UNS2)

      
      uges <- sqrt((uncertTSensCalib*Tch)^2 + u1^2 + u2^2 )/Tch
      
    }
    ccc$Calibration$Analysis$Values$Uncertainty <-
      setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
             "uncertTch",
             "1",
              uges,
             msg[length(msg)])
  }## no ofCo<2
  
  return(ccc)
}

