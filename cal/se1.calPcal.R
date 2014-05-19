se1.calPcal <- function(ccc){
  msg <- "calculated by se1.calPcal()"
  a   <- abbrevList( ccc )

  PFILL   <- getSubList(a$cav$Pressure, "fill")
  pfill   <- getConstVal(NA,NA,PFILL)
  
  PRISE  <- getSubList(a$cav, "rise")
  prise  <- getConstVal(NA, NA, PRISE)


  fcorr   <- getConstVal(a$cav$Expansion,   "corr")
  Tbefore <- getConstVal(a$cav$Temperature, "before")
  Tafter  <- getConstVal(a$cav$Temperature, "after")
  rg      <- getConstVal(a$cav$Correction,  "rg")

  pcal    <- pfill * fcorr * Tafter/Tbefore *(1 - rg)

  if(length(prise) == length(pcal) &
     PFILL$Unit == PRISE$Unit){
      pcal <- pcal + prise
      msg <- paste(msg, "pressure rise corrected (pcal =  pcal + prise)")

  }
  
  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           "cal",
           PFILL$Unit,
           pcal,
           msg)
  return(ccc)
}
