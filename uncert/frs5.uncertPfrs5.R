frs5.uncertPfrs5 <- function(ccc){
  msg <- "Calcilated by frs5.uncertPfrs5()"
  a <- abbrevList(ccc)

  calUnit   <- "Pa"
  resType   <- "cal"
  ## aber:
  if(a$ct == "VG" || a$ct == "IK"){
    resType <- "frs5"
  }

  PFRS         <- getSubList(a$cav$Pressure, resType)
  convpfrs     <- getConvFactor(ccc,calUnit,PFRS$Unit)
  pfrs         <- getConstVal(NA,NA,PFRS)    * convpfrs  # ab hier Pa wenn nicht sowieso

  PFRSRES      <- getSubList(a$cav$Pressure,    "frs5_res")
  convpfrsres  <- getConvFactor(ccc, calUnit, PFRSRES$Unit)
  pfrsres      <- getConstVal(NA,NA,PFRSRES)    * convpfrsres  # ab hier Pa

  ## Hier die Unsicherheiten aus DB:
  ##  (ohne check der Einheit)
  ## Pa ist default
  u1      <- getConstVal(a$cms,   "u_frs5_1")
  u2      <- getConstVal(a$cms,   "u_frs5_2")
  u3      <- getConstVal(a$cms,   "u_frs5_3")
  upres   <- getConstVal(a$cmco,  "u_frs5_res")
  ufrs    <- sqrt(u1*pfrs^2 +
                  u2*pfrs   +
                  u3        +
                  (upres*pfrsres)^2) # in Pa

  resUnit <- "mbar"
  resConv <- getConvFactor(ccc, resUnit, calUnit)
  
  ccc$Calibration$Analysis$Values$Uncertainty <-
    setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
           paste("u_", resType,sep=""),
           resUnit,
           ufrs * resConv,
           msg)


  return(ccc)
}
