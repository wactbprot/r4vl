calError <- function(ccc){
  msg   <- "calculated by calError()"
  a     <- abbrevList(ccc)
  pUnit <- "mbar"

  CAL <- getSubList(a$ca, "cal")
  cal <- getConstVal(NA,NA,CAL)
  
  IND <- getSubList(a$cmv, "p_ind")
  OFS <- getSubList(a$cmv, "p_ind_offset")

  if(is.null(IND)){
    IND <- getSubList(a$cmv, "ind")
  }
  
  if(is.null(OFS)){
    OFS <- getSubList(a$cmv, "ind_offset")
  }
  
  if(is.null(OFS)){
    OFS <- getSubList(a$cmv, "offset")
  }
  
  convInd <- getConvFactor(ccc, CAL$Unit,IND$Unit)
  convOfs <- getConvFactor(ccc, CAL$Unit,OFS$Unit)
  
  ind <- getConstVal(NA,NA,IND) * convInd
  ofs <- getConstVal(NA,NA,OFS) * convOfs

  error <- (ind - ofs)/cal -1
  
  ccc$Calibration$Analysis$Values$Error <-
      setCcl(ccc$Calibration$Analysis$Values$Error, "relative",
             "1",
             error,
             paste(msg, "error is defined by ((p_ind - p_ind_offset)/p_cal-1)")
             )

  return(ccc)
}
