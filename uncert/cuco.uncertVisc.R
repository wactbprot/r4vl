cuco.uncertVisc <- function(ccc){
  msg <- "calculated by cuco.uncertVisc()"
  unit<- "mbar"

  a   <- abbrevList(ccc)

  un <- "uncertVisc"
  
  PCAL  <- getSubList(a$cav, "cal")
  pcal  <- getConstVal(NA,NA,PCAL)
  
  U  <- getSubList(a$cmco1, un)
  u  <- getConstVal(NA,NA, U)

 
  ccc$Calibration$Analysis$Values$Uncertainty <-
      setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
             un,
             "1",
             u * pcal,
             msg)
  return(ccc)
}
