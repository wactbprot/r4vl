ce3.uncertCx <- function(ccc){
  msg <- "calculated by ce3.uncertCx()"

  a   <- abbrevList(ccc)

  PCAL  <- getSubList(a$cav, "cal")
  pcal  <- getConstVal(NA,NA,PCAL)

  noOfPcal   <- length(pcal)
  uncertRes  <- rep(1,noOfPcal)

  if(a$cmscok == "opK1" | a$cmscok == "opK2"| a$cmscok == "opK4" ){

    u1aList <-  getSubList(a$cms, "ce3C1_u1_a")
    u1bList <-  getSubList(a$cms, "ce3C1_u1_b")

    if(PCAL$Unit == "mbar"        &&
       (u1aList$Unit == "1")      &&
       (u1bList$Unit == "1/mbar")  ){

      uncertRes[1:noOfPcal] <-
        sqrt(getConstVal(NA,NA,u1aList)^2 + (getConstVal(NA,NA,u1bList) * pcal)^2)
    }
  }

  if(a$cmscok == "opK3" ){
    uncertRes[1:noOfPcal] <- getConstVal(a$cms, "ce3C2_u1")
  }

  ccc$Calibration$Analysis$Values$Uncertainty <-
    setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
           "uncertCx",
           "1",
           uncertRes,
           msg)

  return(ccc)
}
