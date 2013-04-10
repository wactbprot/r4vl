calVFrs5Se1ExpA <- function(ccc){
  msg <- "calculated by calDFrs5FrsBs()"
  a <- abbrevList(ccc)
  
  calUnit <- "Pa"
  
  pnd     <- getConstVal(a$cmv, "nd")
  pndoff  <- getConstVal(a$cmv, "nd_offset")
  cf      <- getConstVal(a$cmco, "rangeConversionFactor")
  nd      <- (pnd + pndoff) * cf

  Tbefore <- getConstVal(a$cav$Temperature, "before")
  Tafter  <- getConstVal(a$cav$Temperature, "after")
  rg      <- getConstVal(a$cav$Correction,  "rg")

  V1      <- getConstVal(a$cms, "V1")
  V23     <- 21332.28 # nach http://a73434.berlin.ptb.de/mediawiki/index.php/QSE-SE1-2010
  Vnd     <- 80
  pfrs    <- getConstVal(a$cav, "cal")

  PFILL   <- getSubList(a$cav, "fill")

  pfill   <- getConstVal(NA,NA,PFILL)* getConvFactor(ccc,calUnit,PFILL$Unit)

  
  
  V23nd   <- V1*(pfill/(pfrs - nd)*Tafter/Tbefore *(1-rg) - 1)

  A       <- Tbefore/Tafter * 1/(1-rg)*(pfrs - nd)/pfill
  
  V1p     <- (V23 + Vnd)*A/(1-A)

  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           "nd",
           calUnit,
           nd,
           paste(msg, "with (nd-nd_offset)*cf, with cf: ", cf))

  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           "nd_offset",
           calUnit,
           pndoff * cf,
           paste(msg, "with nd_offset * cf, with cf: ", cf))

  
    
  ccc$Calibration$Analysis$Values$Volume <-
    setCcl(ccc$Calibration$Analysis$Values$Volume,
           "V23nd",
           "cm^3",
           V23nd,
           paste(msg, "beinhaltet V2, V3 und Vnd"))

  ccc$Calibration$Analysis$Values$Volume <-
    setCcl(ccc$Calibration$Analysis$Values$Volume,
           "V1p",
           "cm^3",
           V1p,
           paste(msg, "beinhaltet Vnd =",Vnd))

  return(ccc)
}
