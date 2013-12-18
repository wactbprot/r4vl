cuco.uncertOffsetDrift <- function(ccc){
  msg <- "calculated by cuco.uncertOffsetDrift"

  a   <- abbrevList(ccc)

  un <- "uncertOffsetDrift"
  
  PIND  <- getSubList(a$cav, "ind")
  pind  <- getConstVal(NA,NA,PIND)
  
  u  <- getConstVal(a$cmco1, un)

  if(length(u) == 0){
      u <- 1e-10
  }
  
  ccc$Calibration$Analysis$Values$Uncertainty <-
      setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
             un,
             "1",
             u/pind,
             msg)
  
  return(ccc)
}
