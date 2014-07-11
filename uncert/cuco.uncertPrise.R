cuco.uncertPrise <- function(ccc){
  msg <- "calculated by cuco.uncertPrise"
  unit<- "mbar"

  a   <- abbrevList(ccc)

  un <- "uncertPrise"
  
  PRISE  <- getSubList(a$cav, "rise")
  prise  <- getConstVal(NA, NA, PRISE)

  PCAL   <- getSubList(a$cav, "cal")
  pcal   <- getConstVal(NA, NA, PCAL)
  
  U      <- getSubList(a$cmco1, un)
  u      <- getConstVal(NA,NA, U)

  N      <- length(pcal)
  
  if(length(u) == 0){
      u <- 0.15
  }
  if(length(prise) == 0){
      prise <- rep(2e-8, N)
  }

  ures <- u * prise/pcal

  
  ccc$Calibration$Analysis$Values$Uncertainty <-
      setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
             un,
             "1",
             ures,
             msg)
  return(ccc)
}
