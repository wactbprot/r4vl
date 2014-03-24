cuco.uncertExpSd <- function(ccc){
  msg <- "calculated by cuco.uncertExpSd"

  a   <- abbrevList(ccc)

  un <- "uncertExpSd"
  
  
  PIND  <- getSubList(a$cav, "ind")
  pind  <- getConstVal(NA,NA,PIND)

  
  U  <- getSubList(a$cmco1, un)
  u  <- getConstVal(NA, NA, U)
  
  if(length(u) == 0){
      u <- 0.008
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
