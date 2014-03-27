se1.uncertPcal <- function(ccc){
    msg   <- "calculated by  se1.uncertPcal()"
    a     <- abbrevList(ccc)
    
    PCAL  <- getSubList(a$cav$Pressure, "cal")
    pcal  <- getConstVal(NA, NA, PCAL)

    uges  <- sqrt(
        getConstVal(a$cav$Uncertainty, "uncertPfill")   ^2 +  
        getConstVal(a$cav$Uncertainty, "uncertf")       ^2 + 
        getConstVal(a$cav$Uncertainty, "uncertdT")      ^2 + 
        getConstVal(a$cav$Uncertainty, "uncertT1")      ^2 + 
        getConstVal(a$cav$Uncertainty, "uncertRg")      ^2 + 
        getConstVal(a$cav$Uncertainty, "uncertAds")     ^2 + 
        getConstVal(a$cav$Uncertainty, "uncertVz")      ^2 + 
        getConstVal(a$cav$Uncertainty, "uncertGas")     ^2 + 
        getConstVal(a$cav$Uncertainty, "uncertAtm")     ^2 + 
        getConstVal(a$cav$Uncertainty, "uncertValve")   ^2 + 
        getConstVal(a$cav$Uncertainty, "uncertPres")    ^2 + 
        getConstVal(a$cav$Uncertainty, "uncertRep")     ^2 )

  ccc$Calibration$Analysis$Values$Uncertainty <-
    setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
           "uncertPcal_rel",
           "1",
           uges,
           paste(msg, " (k=1)"))

  ccc$Calibration$Analysis$Values$Uncertainty <-
    setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
           "uncertPcal_abs",
           PCAL$Unit,
           uges * pcal,
           paste(msg, " (k=1)"))
  
    return(ccc)
}
