fm1.uncertDPfill <- function(ccc){
  msg <- "Calculated by fm1.uncertDPfill()"
  
  a <- abbrevList(ccc)
  
  PFILL  <- getSubList(a$cav, "fill")
  pfill  <- getConstVal(NA,NA,PFILL)
  
  Tfm1   <- getConstVal(a$cav, "fm1")
  Troom  <- getConstVal(a$cav, "room")

  ## Drücke die nicht in Bereich fallen,
  ## haben die Unsicherheit durch term Transp. nicht
  ## d.h. der result vektor kann mit 0en initialisiert werden:
  uncertDPfillTherm  <- rep(0,length(pfill))
  UTHERMTRANS      <-  getSubList(a$cms,"fm1ThermTrans_u1")
  ipfill           <- checkUncertRange(UTHERMTRANS, PFILL)
  
  if((length(ipfill) > 0) && (!(ipfill[1] == 0))){
    
    
    
    F1    <- abs(1 - sqrt( Tfm1/Troom ))
    F2    <- getConstVal(NA,NA,UTHERMTRANS) 
    
    uncertDPfillTherm[ipfill] <- abs(F1[ipfill] * F2 * (1 + 2 * log( 0.1/pfill[ipfill] )))
    
    msg <- paste(msg,
                 ", therm. transp. corr. at point(s)",
                 length(ipfill),
                 "with DT= mean(abs(Tfm1 - Troom)) = ",
                 mean(abs(Tfm1 - Troom)))
    
  }
  
  uncertDPfillRes <- sqrt(
                       uncertDPfillTherm^2 +
                       getConstVal(a$cms,"fm1TimeDiff_u2")^2 +
                       getConstVal(a$cms,"fm1ValveClose_u3")^2 +
                       getConstVal(a$cms,"fm1Sync_u4")^2
                       )
  
  
  ccc$Calibration$Analysis$Values$Uncertainty <-
    setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
           "uncertDPfill",
           "1",
           uncertDPfillRes,
           paste(msg,
                 "considers:fm1ThermTrans_u1, fm1TimeDiff_u2, fm1ValveClose_u3 and fm1Sync_u4") )
  
  return(ccc)
}
