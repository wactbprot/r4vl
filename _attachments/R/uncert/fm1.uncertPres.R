fm1.uncertPres <- function(ccc){
  msg <- "Calculated by fm1.uncertPres()"
  
  a <- abbrevList(ccc)
  
  PFILL  <- getSubList(a$cav, "fill")
  pfill  <- getConstVal(NA,NA,PFILL)
  
  ## wird mit 1 initialisiert
  ## um auf bereich pfill  < 1Pa
  ## aufmerksam zu werden
  ## ist hier noch nichtbearb.
  uncertPres  <- rep(1,length(pfill))
  UPRES       <-  getSubList(a$cms,"fm1Pres_u1")
  ipfill      <- checkUncertRange(UPRES, PFILL)
  
  if((length(ipfill) > 0) && (!(ipfill[1] == 0))){
    k        <- getConvFactor(ccc,UPRES, PFILL)
    upres    <- getConstVal(NA,NA,UPRES)/(pfill*k) 
  }
  
  ccc$Calibration$Analysis$Values$Uncertainty <-
    setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
           "uncertPres",
           "1",
           upres,
           msg)
  
  return(ccc)
}
