cuco.uncertPind <- function(ccc){
  msg <- "calculated by cuco.uncertPind"

  a   <- abbrevList(ccc)
  
  PIND  <- getSubList(a$cav, "ind")
  pind  <- getConstVal(NA,NA,PIND)

  u1 <- getConstVal(a$cav$Uncertainty, "uncertDigit")
  u2 <- getConstVal(a$cav$Uncertainty, "uncertExpSd")
  u3 <- getConstVal(a$cav$Uncertainty, "uncertGasPurity")
  u4 <- getConstVal(a$cav$Uncertainty, "uncertOffsetDrift")
  u5 <- getConstVal(a$cav$Uncertainty, "uncertOffset")
  u6 <- getConstVal(a$cav$Uncertainty, "uncertSync")

  uncertges <- sqrt(u1^2 +
                    u2^2 +
                    u3^2 +
                    u4^2 +
                    u5^2 +
                    u6^2)
  
  ccc$Calibration$Analysis$Values$Uncertainty <-
    setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
           "uncertPind_rel",
           "1",
           uncertges,
           paste(msg, " (k=1)"))

  ccc$Calibration$Analysis$Values$Uncertainty <-
    setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
           "uncertPind_abs",
           PIND$Unit,
           uncertges * pind,
           paste(msg, " (k=1)"))

  return(ccc)
}
