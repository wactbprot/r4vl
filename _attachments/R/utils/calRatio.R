calRatio <- function(ccc){
  msg <- "calculated by calError()"

  a <- abbrevList(ccc)

  IND <- getSubList(a$cm, "p_ind")
  OFS <- getSubList(a$cm, "p_ind_offset")

  if(is.null(IND)){
    IND <- getSubList(a$cm, "ind")
  }
  
  if(is.null(OFS)){
    OFS <- getSubList(a$cm, "ind_offset")
  }
  
  if(is.null(OFS)){
    OFS <- getSubList(a$cm, "offset")
  }

  CAL <- getSubList(a$ca, "cal")
  if(is.null(CAL)){
    CAL <- getSubList(a$ca, "p_cal")
  }

  convInd <- getConvFactor(ccc, CAL,IND)
  convOfs <- getConvFactor(ccc, CAL,OFS)

  ind <- getConstVal(NA,NA,IND) * convInd
  ofs <- getConstVal(NA,NA,OFS) * convOfs
  
  msg <- paste(msg,
               "ind conversion factor was:", convInd,
               "offset conversion factor was:",convOfs )


  if(length(a$cmscoi) > 0){
    if(a$cmscoi[1] > 0){
      ind <- ind[-a$cmscoi]
      ofs <- ofs[-a$cmscoi]
    }
  }

  cal <- getConstVal(NA,NA,CAL)
  corrind <- (ind - ofs)
  ratio <- corrind/cal

  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure, "p_ind",
           "mbar",
           ind,
           paste(msg)
           )

  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure, "p_ind_offset",
           "mbar",
           ofs,
           paste(msg)
           )

  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure, "p_ind_corr",
           "mbar",
           corrind,
           paste(msg, ";p_ind-p_ind_offset")
           )

  ccc$Calibration$Analysis$Values$Ratio <-
    setCcl(ccc$Calibration$Analysis$Values$Error, "error",
           "1",
           ratio,
           paste(msg, " ;ratio is defined as p_ind_corr/p_cal")
           )
  ccc$Calibration$Analysis$Values$Error <-
    setCcl(ccc$Calibration$Analysis$Values$Error, "relative",
           "%",
           (corrind/cal-1)*100,
           paste(msg, ";calculated by (p_ind_corr/p_cal - 1)*100")
           )
  
  return(ccc)
}
