ce3.uncertTotal <- function(ccc){
  msg <- "calculated by ce3.uncertTotal"

  a   <- abbrevList(ccc)

  u1 <- getConstVal(a$cav$Uncertainty, "uncertPcal_rel")
  u2 <- getConstVal(a$cav$Uncertainty, "uncertPind_rel")

  u <- sqrt(u1^2 + u2^2)
  
  ccc$Calibration$Analysis$Values$Uncertainty <-
      setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
             "uncertTotal_rel",
             "1",
             u,
             paste(msg, " (k=1)"))  
  
  return(ccc)
  
}
