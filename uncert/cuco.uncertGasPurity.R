cuco.uncertGasPurity <- function(ccc){
  msg <- "calculated by cuco.uncertGasPurity"

  a   <- abbrevList(ccc)

  un <- "uncertGasPurity"
  
  
  PIND  <- getSubList(a$cav, "ind")
  pind  <- getConstVal(NA,NA,PIND)
  
  U  <- getSubList(a$cmco1, un)
  u  <- getConstVal(NA, NA, U)

  if(length(u) == 0){
        u <- 0.005 
        U$Unit <- "1"
    }
  if(U$Unit == "mbar"){
      u <- u/pind
  }
  ccc$Calibration$Analysis$Values$Uncertainty <-
      setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
             un,
             "1",
             u,
             msg)
  
  return(ccc)
}
