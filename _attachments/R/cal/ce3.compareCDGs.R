ce3.compareCDGs <- function(ccc){
  msg    <- "calculated by ce3.compareCDGs"
  a      <- abbrevList(ccc)
 
  ## Range u.ä. ist im recipe
  ## festgelegt
  pcdga    <-  getConstVal(a$cma$Pressure, "cdga_comp") #10T
  pcdgb    <-  getConstVal(a$cma$Pressure, "cdgb_comp") #1000T
  p0cdga   <-  getConstVal(a$cma$Pressure, "cdga_comp_offset")
  p0cdgb   <-  getConstVal(a$cma$Pressure, "cdgb_comp_offset")

  dpfill  <- (pcdga  - p0cdga)/(pcdgb  - p0cdgb) -1

  
  ccc$Calibration$Analysis <- checkSetList(
                                ccc$Calibration$Analysis)
  
  ccc$Calibration$Analysis$AuxValues <- checkSetList(
                                ccc$Calibration$Analysis$AuxValues)
  
  
  ccc$Calibration$Analysis$AuxValues$Pressure <-
    setCcl(ccc$Calibration$Analysis$AuxValues$Pressure,
           "dpfill",
           "1",
           dpfill,
           paste(msg, "; dpfill =  (pcdga  - p0cdga)/(pcdgb  - p0cdgb) -1") )

  return(ccc)
}
