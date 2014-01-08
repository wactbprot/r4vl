cuco.uncertPOffset <- function(ccc){
  msg <- "calculated by cuco.uncertPOffset"
  unit<- "mbar"

  a   <- abbrevList(ccc)

  un <- "uncertOffset"
  
  PIND  <- getSubList(a$cav, "ind")
  pind  <- getConstVal(NA,NA,PIND)
  
  U  <- getSubList(a$cmco1, un)
  u  <- getConstVal(NA,NA, U)

  if(length(u) == 0){
      u <- 1e-10
      U <- list(Unit = "mbar")
  }

  convu <- getConvFactor(ccc,PIND$Unit,U$Unit)

  ccc$Calibration$Analysis$Values$Uncertainty <-
      setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
             un,
             "1",
             u * convu/pind,
             msg)
  return(ccc)
}
