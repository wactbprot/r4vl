fm1.uncertDeltat <- function(ccc){

  msg <- "Calculated by fm1.uncertDeltat()"

  a <- abbrevList(ccc)
  
  PFILL     <- getSubList(a$cav, "fill")
  pfill     <- getConstVal(NA,NA,PFILL)
  pfillUnit <- PFILL$Unit
  noOfPfill <- length(pfill)

  uncertRes <- rep(1,length(pfill))

  u1List <- getSubList(a$cms,"fm1Deltat_u1")
  u1     <- getConstVal(NA,NA,u1List)
  iu1    <- checkUncertRange(u1List, PFILL)
  
  u2List <- getSubList(a$cms,"fm1Deltat_u2")
  u2     <- getConstVal(NA,NA,u2List)
  iu2    <- checkUncertRange(u2List, PFILL)
 
  u3List <- getSubList(a$cms,"fm1Deltat_u3")
  u3     <- getConstVal(NA,NA,u3List)
  iu3    <- checkUncertRange(u3List, PFILL)
  
  if((length(iu1) == length(iu2)) &
     (length(iu1) == length(iu3))){
    uncertRes[iu1] <- sqrt(u1^2+u2^2+u3^2)
    
    
    ccc$Calibration$Analysis$Values$Uncertainty <-
    setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
           "uncertDeltat",
           "1",
           uncertRes,
           msg)
    
    return(ccc)
    
  }else{
    stop("range don't match in fm1.uncertDeltat()")
  }
}
