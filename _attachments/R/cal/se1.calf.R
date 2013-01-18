se1.calf <- function(ccc){
  msg     <- "calculated by se1.calf()"
  volUnit <- "cm^3" 
  
  a    <- abbrevList(ccc)
  ## -------------------- zusatzvolumen aufaddieren  v 
  if( length(a$cmsc$Volume$Type) == 0){
    Vz <- 0
    for(i in 1:length(a$cmsc$Volume)){
      
      fromUnit <- a$cmsc$Volume[[i]]$Unit
      conv     <- getConvFactor(ccc,volUnit, fromUnit)
      msg      <- paste(msg, "used", conv, "to convert from",fromUnit, "to", volUnit)

      ## ---> ToDo: testen
      ##  Vz   <- Vz + a$cmsc$Volume[[i]]$Value[length(a$cmsc$Volume[[i]]$Value)]
      ##
      Vz       <- Vz + getConstVal(NA,NA,a$cmsc$Volume[[i]]) * conv
    }
    
  }else{
    
    fromUnit <- a$cmsc$Volume$Unit
    conv     <- getConvFactor(ccc,volUnit,  fromUnit)
    msg      <- paste(msg, "used", conv, "to convert from",fromUnit, "to", volUnit)
    
    Vz       <- getConstVal(NA,NA,a$cmsc$Volume) * conv
    
  }
  
  ## -------------------- vector mit expansiossequenzen erzeugen
  
  N  <- length(a$cmscex)
  f  <- rep(NA,N)
  sv <- rep(NA,N) ## start volumen
  
  if(length(a$cmscex) > 0){
    for(i in 1:N){
      ## die ges. substruktur
      SCI <- getSubList(a$cms,a$cmscex[[i]])
      ## der Value
      f[i] <- getConstVal(NA,NA,SCI)
      ## Startvolumen mit ber. der Einheiten
      
      SV   <- getSubList(a$cms, SCI$StartVolume)
      conv <- getConvFactor(ccc,volUnit, SV$Unit)
      
      sv[i] <- getConstVal(NA,NA,SV) * conv
      
      
      fp <- 1/(1/f + Vz/sv)
      
    }
    
    fp <-  checkOutIndex(a,fp)
    
    ccc$Calibration$Analysis$Values$Expansion <-
      setCcl(ccc$Calibration$Analysis$Values$Expansion,
             "corr",
             "1",
             fp,
             msg)
    
    return(ccc)
  }
}
