ce3.uncertCx <- function(ccc){
  msg <- "calculated by ce3.uncertCx()"

  tmpAn     <- ccc$Calibration$Analysis
  tmpMea    <- ccc$Calibration$Measurement
  tmpStrd   <- tmpMea$Standard

  operationKind <- tmpMea$SequenzControl$operationKind

  pcalList   <- getSubList(tmpAn, "cal")
  pcal       <- as.double(pcalList$Value)
  noOfPcal   <- length(pcal)
  uncertRes  <- rep(1,noOfPcal)


  if(operationKind == "opK1" | operationKind == "opK2"| operationKind == "opK4" ){

   u1aList <-  getSubList(tmpStrd, "ce3C1_u1_a")
   u1bList <-  getSubList(tmpStrd, "ce3C1_u1_b")

   if((u1aList$Unit == "1")      &
      (u1bList$Unit == "1/mbar") &
      (pcalList$Unit == "mbar")  ){
     uncertRes[1:noOfPcal] <- sqrt(getConstVal(NA,NA,u1aList)^2 +
                                   (getConstVal(NA,NA,u1bList) * pcal)^2)
   }
  }

  if(operationKind == "opK3" ){
    uncertRes[1:noOfPcal] <- getConstVal(tmpStrd, "ce3C2_u1")
  }

  ccc$Calibration$Analysis$Values$Uncertainty <- setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
                                                        "uncertCx",
                                                        "1",
                                                        uncertRes,
                                                        msg)
  return(ccc)
}
