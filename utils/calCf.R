calCf <- function(ccc){
  msg <- "calculated by calError()"

  a <- abbrevList(ccc)

  IND <- getSubList(a$cmv, "p_ind")
  OFS <- getSubList(a$cmv, "p_ind_offset")

  CAL <- getSubList(a$ca, "p_cal")


  
  convInd <- getConvFactor(ccc, CAL,IND)
  convOfs <- getConvFactor(ccc, CAL,OFS)

  ind <- getConstVal(NA,NA,IND) * convInd
  ofs <- getConstVal(NA,NA,OFS) * convOfs
  
  msg <- paste(msg,
               "ind conversion factor was:", convInd,
               "offset conversion factor was:",convOfs )

  ind  <- checkOutIndex(a,ind)
  ofs  <- checkOutIndex(a,ofs)

  cal <- getConstVal(NA,NA,CAL)
  corrind <- (ind - ofs)
 
  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           "p_ind",
           "mbar",
           ind,
           paste(msg)
           )

  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           "p_ind_offset",
           "mbar",
           ofs,
           paste(msg)
           )

  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           "p_ind_corr",
           "mbar",
           corrind,
           paste(msg, ";corrind is (p_ind - p_ind_offset)")
           )

  ccc$Calibration$Analysis$Values$CorrectionFactor <-
    setCcl(ccc$Calibration$Analysis$Values$CorrectionFactor,
           "correctionFactor",
           "1",
           cal/corrind,
           paste(msg, ";correctionFactor is defined by cal/p_ind_corr")
           )
  
  ccc$Calibration$Analysis$Values$Error <-
    setCcl(ccc$Calibration$Analysis$Values$Error,
           "relative",
           "%",
           (corrind/cal-1)*100,
           paste(msg, ";error is defined by (p_ind_corr/p_cal-1)*100")
           )
  return(ccc)
}
