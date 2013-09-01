fm1.uncertPres <- function(ccc){
  msg <- "Calculated by fm1.uncertPres()"
  
  a <- abbrevList(ccc)
  
  PFILL  <- getSubList(a$cav, "fill")
  pfill  <- getConstVal(NA,NA,PFILL)
  
  ## wird mit 1 initialisiert
  ## um auf Bereich pfill  < 1Pa
  ## aufmerksam zu werden
  ## ist hier noch nichtbearb.
  upres <- rep(1,length(pfill))
  
  UPRESA       <-  getSubList(a$cms,"fm1Pres_u1_a")
  il           <- checkUncertRange(UPRESA, PFILL)
  
  if((length(il) > 0) & (!(il[1] == 0))){
    k            <- getConvFactor(ccc,UPRESA, PFILL)
    upres[il]    <- getConstVal(NA,NA,UPRESA)/(pfill[il]*k) 
  }
  
  UPRESB       <-  getSubList(a$cms,"fm1Pres_u1_b")
  ih           <- checkUncertRange(UPRESB, PFILL)
  
  if((length(ih) > 0) & (!(ih[1] == 0))){
    k            <- getConvFactor(ccc,UPRESB, PFILL)
    upres[ih]    <- getConstVal(NA,NA,UPRESB)/(pfill[ih]*k) 
  }
  
  ccc$Calibration$Analysis$Values$Uncertainty <-
    setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
           "uncertPres",
           "1",
           upres,
           msg)
  
  return(ccc)
}
