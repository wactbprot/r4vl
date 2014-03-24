ce3.uncertTotal <- function(ccc){
  msg <- "calculated by ce3.uncertTotal"

  a   <- abbrevList(ccc)

  PCAL  <- getSubList(a$cav, "cal")
  pcal  <- getConstVal(NA,NA,PCAL)

  u1 <- getConstVal(a$cav$Uncertainty, "uncertPcal_rel")
  u2 <- getConstVal(a$cav$Uncertainty, "uncertPind_rel")

  u <- sqrt(u1^2+u2^2)
  
  ccc$Calibration$Analysis$Values$Uncertainty <-
      setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
             "uncertTotal_rel",
             "1",
             u,
             paste(msg, " (k=1)"))
  
  ccc$Calibration$Analysis$Values$Uncertainty <-
      setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
             "uncertTotal_abs",
             PCAL$Unit,
             u * pcal,
             paste(msg, " (k=1)"))
  
  return(ccc)
  
}
