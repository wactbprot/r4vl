calDFrs5FrsBs <- function(ccc){
  msg <- "calculated by calDFrs5FrsBs()"
  a <- abbrevList(ccc)
  
  calUnit <- "Pa"
  
  ## 
  PFRS5  <- getSubList(a$cav, "frs5")
  PFRSBs <- getSubList(a$cav, "frs_bs")
  
  pnd     <- getConstVal(a$cmv, "nd")
  pndoff  <- getConstVal(a$cmv,"nd_offset")
  
  cf <- getConstVal(a$cms, "confFactor")

  nd <- (pnd-pndoff)*cf
  
  pfrs5  <- getConstVal(NA,NA,PFRS5) * getConvFactor(ccc,calUnit,PFRS5$Unit)
  pfrsBs <- getConstVal(NA,NA,PFRSBs) * getConvFactor(ccc,calUnit,PFRSBs$Unit)
  
  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           "d",
           calUnit,
           pfrsBs - pfrs5 ,
           paste(msg, "with p_frs_bs - p_frs_bln"))

  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           "nd",
           calUnit,
           nd,
           paste(msg, "with (pnd-pndoff)*cf, with cf: ", cf))

  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           "d_nd",
           calUnit,
           pfrsBs - pfrs5 - nd ,
           paste(msg, "with p_frs_bs - p_frs_bln - nd"))

  ccc$Calibration$Analysis$Values$Error <-
    setCcl(ccc$Calibration$Analysis$Values$Error,
           "relative",
           "1",
           pfrsBs/pfrs5 - 1,
           paste(msg, "with p_frs_bs / p_frs_bln - 1"))

  ccc$Calibration$Analysis$Values$Error <-
    setCcl(ccc$Calibration$Analysis$Values$Error,
           "relative_nd",
           "1",
           pfrsBs/(pfrs5 - nd) - 1,
           paste(msg, "with p_frs_bs / (p_frs_bln - nd) - 1"))
  
  return(ccc)
}
