ce3.uncertF <- function(ccc){
  
  msg <- "calculated by ce3.uncertF()"

  a <- abbrevList(ccc)
  
  pcal      <- getConstVal(a$ca, "cal")
  noOfPcal  <- length(pcal)

  uncertRes  <- rep(1,noOfPcal)
  
  uncertRes[1:noOfPcal]  <-  getConstVal(a$cms, "ce3F_u1")
  
  ccc$Calibration$Analysis$Values$Uncertainty <-
    setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
           "uncertF",
           "1",
           uncertRes,
           msg)
  
  return(ccc)
}
