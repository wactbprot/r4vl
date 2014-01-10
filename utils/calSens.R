calSens <- function( ccc ){
  
  msg   <- "calculated by calSens()"
  a     <- abbrevList(ccc)
  iUnit <- "A"

  IE    <- getSubList(a$cmco1, "ie")
  iconv <- getConvFactor(ccc,iUnit, IE$Unit)
  ie    <- getConstVal(NA, NA, IE) * iconv 
  
  ioff  <- getConstVal(a$cav, "ind_offset")
  iind  <- getConstVal(a$cav, "ind")
  pcal  <- getConstVal(a$cav, "cal")

  
  ccc$Calibration$Analysis$Values$Sensitivity<-
      setCcl(ccc$Calibration$Analysis$Values$Sensitivity, "gauge_sens",
             "A/mbar/mA",
             (iind - ioff)/(pcal * ie) ,
             paste(msg, "Ie is given in 1st calib. obj. with: ", ie,IE$Unit))
  
  return( ccc )
}
