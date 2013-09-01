ce3.uncertQsplit <- function(ccc){
  
  msg <- "calculated by ce3.uncertQsplit()"

  a <- abbrevList(ccc)

  operationKind <- a$cmscok
  
  pcal   <- getConstVal(a$ca, "cal")
  noOfPcal   <- length(pcal)
  
  uncertRes  <- rep(1,noOfPcal)
  
  
  if(operationKind == "opK1" | operationKind == "opK2"| operationKind == "opK4" ){
    uncertRes[1:noOfPcal]   <-  getConstVal(a$cms, "ce3qsplit_u1_a")
  }
  
  if(operationKind == "opK3" ){
    uncertRes[1:noOfPcal]   <-  getConstVal(a$cms, "ce3qsplit_u1_b")
  }
  
  ccc$Calibration$Analysis$Values$Uncertainty <-
    setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
           "uncertQsplit",
           "1",
           uncertRes,
           msg)
  
  return(ccc)
}
