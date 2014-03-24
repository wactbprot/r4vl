se2.calPfill <- function(ccc){
  msg <- "calculated by se2.calPfill()"
  
  a     <- abbrevList( ccc )
  pUnit <- "mbar"
  
  PFILL       <- getSubList(a$cmv, "fill") ##kPa
  pfill       <- getConstVal(NA, NA, PFILL)

  N           <- length(pfill)
  
  PFILLOFFSET <- getSubList(a$cma, "fill_offset") ##kPa
  offs        <- getConstVal(NA, NA, PFILLOFFSET)
  
  omt         <- getConstVal(a$cma, "amt_offset")
  bmt         <- getConstVal(a$cmv, "amt_before")
  
  if(!is.null(omt) & !is.null(bmt) & 
     (length(omt) > 1) & (length(bmt) > 1)){
      
      offset <- rep(NA,N)
      
      for(i in 1:length(omt)){
          k         <- which(bmt > omt[i])
          offset[k] <- offs[i] 
      }
  }else{
      offset <- rep(offs, N)
  }
 
  cfQbs        <- list()
  cfQbs$a      <- getConstVal(  a$cmco,"qbsCorrA")
  cfQbs$b      <- getConstVal(  a$cmco,"qbsCorrB")
  cfQbs$c      <- getConstVal(  a$cmco,"qbsCorrC")
  cfQbs$d      <- getConstVal(  a$cmco,"qbsCorrD")

  pfill.uncorr.mbar   <- (pfill - offset) * getConvFactor(ccc, pUnit,  PFILL$Unit)
  
  pfill.mbar         <- pfill.uncorr.mbar/(fn.3132(cfQbs, pfill.uncorr.mbar)/100 + 1)
    
  ccc$Calibration$Analysis$Values$Pressure <-
      setCcl(ccc$Calibration$Analysis$Values$Pressure,
             "fill",
             pUnit,
             pfill.mbar,
             msg)
  return(ccc)

}

