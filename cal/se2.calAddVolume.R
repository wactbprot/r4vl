se2.calAddVolume <- function(ccc){
    msg     <- "calculated by se2.calAddVolume()"
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
    
    ccc$Calibration$Analysis$Values$Volume <-
        setCcl(ccc$Calibration$Analysis$Values$Volume,
               "add",
               "1",
               Vz,
               msg)
      
    return(ccc)
}
