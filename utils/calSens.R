calSens <- function( ccc ){
  
  msg   <- "calculated by calSens()"
  a     <- abbrevList(ccc)
  iUnit <- "A"

  IE    <- getSubList(a$cmco1, "ie")
  iconv <- getConvFactor(ccc,iUnit, IE$Unit)
  ie    <- getConstVal(NA, NA, IE) * iconv
  
  ioff  <- getConstVal(a$cmv, "ind_offset")
  icor  <- getConstVal(a$cmv, "ind_corr")
  iind  <- getConstVal(a$cmv, "ind")
  pcal  <- getConstVal(a$cav, "cal")

    ccc$Calibration$Analysis$Values$Error<-
      setCcl(ccc$Calibration$Analysis$Values$Error, "relative",
             "1",
             (iind - ioff)/(pcal * ie) ,
             paste(msg, "Ie is given in 1st calib. obj. with: ", ie,IE$Unit))
  
  return( ccc )
}
