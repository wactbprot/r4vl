fm3.uncertDPfill <- function(ccc){
  msg <- "Calculated by fm3.uncertDPfill()"
  
  a <- abbrevList(ccc)
  
  PFILL  <- getSubList(a$cav, "fill")
  pfill  <- getConstVal(NA,NA,PFILL)
  
  Tfm3   <- getConstVal(a$cav, "Tfm3")
  Tpbox  <- getConstVal(a$cav, "Tpbox")

  ## Drücke die nicht in Bereich fallen,
  ## haben diese Unsicherheit nicht
  ## d.h. der result vektor kann mit 0en initialisiert werden:

  uncertDPfillRes  <- rep(0,length(pfill))
  
  UTHERMTRANS      <-  getSubList(a$cms,"fm3ThermTrans_u1")
  
  ipfill           <- checkUncertRange(UTHERMTRANS, PFILL)
  
  if((length(ipfill) > 0) & (!(ipfill[1] == 0))){
    
    
    
    F1    <- abs(1 - sqrt( Tfm3/Tpbox ))
    F2    <- getConstVal(NA,NA,UTHERMTRANS) 
    
    ## Gleichung s. [[QSE-FM3-98_10#Unsicherheiten_durch_Abweichen_des
    ## _tats.C3.A4chlichen_Drucks_vom_gemessen_Druck]]
    
    uncertDPfillRes[ipfill] <- abs(F1[ipfill] * F2 * (1 + 2 * log( 0.1/pfill[ipfill] )))
    
    msg <- paste(msg,
                 ", therm. transp. corr. at point(s)",
                 ipfill,
                 "with DT= abs(Tfm3 - Tpbox) = ",
                 abs(Tfm3 - Tpbox))
    
  }
  
  
  ccc$Calibration$Analysis$Values$Uncertainty <-
    setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
           "uncertDPfill",
           "1",
           uncertDPfillRes,
           msg)
  
  return(ccc)
}
