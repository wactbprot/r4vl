cuco.uncertPrise <- function(ccc){
  msg <- "calculated by cuco.uncertPrise"
  unit<- "mbar"

  a   <- abbrevList(ccc)

  un <- "uncertPrise"
  
  PIND  <- getSubList(a$cav, "ind")
  pind  <- getConstVal(NA,NA,PIND)
  
  U  <- getSubList(a$cmco1, un)
  u  <- getConstVal(NA,NA, U)

  if(length(u) == 0){
      if(PIND$Unit == "mbar"){
          u <- 5.6e-10
          U <- list(Unit = "mbar")
      }
  }

  ccc$Calibration$Analysis$Values$Uncertainty <-
      setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
             un,
             "1",
             u / pind,
             msg)
  return(ccc)
}
