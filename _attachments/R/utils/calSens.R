calSens <- function( ccc ){

  msg <- "calculated by calSens()"
  a   <- abbrevList(ccc)

  ie   <- 0.305e-3
  ioff <- getConstVal(a$cmv, "offset")
  iind <- getConstVal(a$cmv, "ind")
  pcal <- getConstVal(a$cav, "cal")

    ccc$Calibration$Analysis$Values$Error<-
      setCcl(ccc$Calibration$Analysis$Values$Error, "relative",
             "1",
             (iind - ioff)/(pcal * ie) ,
             msg)
  
  return( ccc )
}
