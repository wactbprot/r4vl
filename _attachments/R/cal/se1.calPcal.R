se1.calPcal <- function(ccc){
  msg <- "calculated by calPcal()"

  a <- abbrevList( ccc )

  ## kommt alles aus Analysis struct
  ## es ist deshalb kein checkOutIndex mehr nötig
  PFILL <- getSubList(a$cav$Pressure, "fill")
  pfill   <- getConstVal(NA,NA,PFILL)


  fcorr   <- getConstVal(a$cav$Expansion, "corr")
  Tbefore <- getConstVal(a$cav$Temperature, "before")
  Tafter  <- getConstVal(a$cav$Temperature, "after")
  rg      <- getConstVal(a$cav$Correction, "rg")

  pcal <- pfill * fcorr * Tafter/Tbefore *(1-rg)

  ccc$Calibration$Analysis$Values$Pressure <-
      setCcl(ccc$Calibration$Analysis$Values$Pressure,
             "cal",
             PFILL$Unit,
             pcal,
             msg)
  return(ccc)
}
