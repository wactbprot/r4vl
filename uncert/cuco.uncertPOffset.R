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
      if(PIND$Unit == "mbar"){
          u <- 1e-10
          U <- list(Unit = "mbar")
      }
      if(PIND$Unit == "A"){
          u <- 1e-13
          U <- list(Unit = "A")
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
