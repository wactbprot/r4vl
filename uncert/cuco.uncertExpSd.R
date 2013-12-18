cuco.uncertExpSd <- function(ccc){
  msg <- "calculated by cuco.uncertExpSd"

  a   <- abbrevList(ccc)

  un <- "uncertExpSd"
  
  
  PIND  <- getSubList(a$cav, "ind")
  pind  <- getConstVal(NA,NA,PIND)
  
  u  <- getConstVal(a$cmco1, un)

  if(length(u) == 0){
      u <- 0.008 * pind
  }
  
  ccc$Calibration$Analysis$Values$Uncertainty <-
      setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
             un,
             "1",
             u/pind,
             msg)
  
  return(ccc)
}
