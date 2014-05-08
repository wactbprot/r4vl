cuco.uncertEmis <- function(ccc){
  msg <- "calculated by cuco.uncertExpSd"

  a   <- abbrevList(ccc)

  un <- "uncertEmis"
  
  
  PIND  <- getSubList(a$cav, "ind")
  pind  <- getConstVal(NA,NA,PIND)

  
  U  <- getSubList(a$cmco1, un)
  u  <- getConstVal(NA, NA, U)
  
  if(length(u) == 0){
      u <- 0.002
      U$Unit <- "1"
  }
  if(length(u) == 1){
      u <- rep(u, length(pind))
  }
  ccc$Calibration$Analysis$Values$Uncertainty <-
      setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
             un,
             "1",
             u,
             msg)
  
  return(ccc)
}
