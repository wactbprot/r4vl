se1.yamp.calf <- function(ccc){
  msg     <- "calculated by se1.yamp.calf()"
  volUnit <- "cm^3" 
  Vz      <- 0  
  a       <- abbrevList(ccc)

  ## -------------------- Zusatzvolumen aufaddieren  v 
  for(i in 1:length(a$cma$Volume)){
    
    fromUnit <- a$cma$Volume[[i]]$Unit
    conv     <- getConvFactor(ccc,volUnit, fromUnit)
    msg      <- paste(msg, "used", conv, "to convert from",fromUnit, "to", volUnit)
    
    nV       <- getConstVal(NA,NA,a$cma$Volume[[i]])
    lnV      <- length(nV) # nur die letzte Eingabe zählt
    
    Vz       <- Vz + nV[lnV] * conv
  }

  ## -------------------- f gibts in yamp for free
  f        <- getConstVal(a$cmv$Expansion, "ratio_uncorr")
  
  SV       <- getSubList(a$cmv$Expansion, "volume_start")
  fromUnit <- SV$Unit
  conv     <- getConvFactor(ccc,volUnit, fromUnit)
  
  sV       <- getConstVal(NA,NA,SV)*conv
  
  fp       <- 1/(1/f + Vz/sV)
  
    ccc$Calibration$Analysis$Values$Expansion <-
      setCcl(ccc$Calibration$Analysis$Values$Expansion,
             "corr",
             "1",
             fp,
             msg)
    
    return(ccc)

}
