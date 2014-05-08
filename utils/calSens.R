calSens <- function( ccc ){
  
  msg   <- "calculated by calSens()"
  a     <- abbrevList(ccc)
  iUnit <- "A"


  
  IE    <- getSubList(a$cmco1, "ie")
  iconv <- getConvFactor(ccc,iUnit, IE$Unit)
  ie    <- getConstVal(NA, NA, IE) * iconv 
  
  OFF  <- getSubList(a$cav, "ind_offset")
  IND  <- getSubList(a$cav, "ind")
  PCAL  <- getSubList(a$cav, "cal")

  
  ioff  <- getConstVal(NA, NA, OFF )
  iind  <- getConstVal(NA, NA, IND )
  pcal  <- getConstVal(NA, NA, PCAL )


  if(OFF$Unit == IND$Unit &
     IND$Unit == iUnit &
     PCAL$Unit == "mbar"){

      ccc$Calibration$Analysis$Values$Sensitivity <-
          setCcl(ccc$Calibration$Analysis$Values$Sensitivity, "gauge_sens",
             "1/mbar",
                 (iind - ioff)/(pcal * ie) ,
                 paste(msg, "Ie is given in 1st calib. obj. with: ", ie,IE$Unit))
  }
  
  return( ccc )
}
