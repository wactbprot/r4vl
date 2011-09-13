frsbs.uncertPfrsbs <- function(ccc){
  msg <- "Calcilated by frsbs.uncertPfrsbs()"
  a <- abbrevList(ccc)

  calUnit   <- "Pa"
  resType <- "cal"
  ## aber:
  if(a$ct == "VG" || a$ct == "IK"){
    resType <- "frsbs"
  }

  PFRS      <- getSubList(a$cav$Pressure, resType)
  convpfrs   <- getConvFactor(ccc,calUnit,PFRS$Unit)
  pfrs        <- getConstVal(NA,NA,PFRS)    * convpfrs  # ab hier Pa wenn nicht sowieso

  PFRSRES      <- getSubList(a$cav$Pressure,    "frsbs_res")
  convpfrsres   <- getConvFactor(ccc,calUnit,PFRSRES$Unit)
  pfrsres        <- getConstVal(NA,NA,PFRSRES)    * convpfrsres  # ab hier Pa wenn nicht sowieso

  ## hier die Unsicherheiten aus DB:
  ##  (ohne check der Einheit)

  u1      <- getConstVal(a$cms,   "u_frsbs_1")
  u2      <- getConstVal(a$cms,   "u_frsbs_2")
  u3      <- getConstVal(a$cms,   "u_frsbs_3")
  upres   <- getConstVal(a$cmco,  "u_frsbs_res")
  ufrs <- sqrt(u1*pfrs^2 + u2*pfrs + u3 + (upres*pfrsres)^2)


  ccc$Calibration$Analysis$Values$Uncertainty <-
    setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
           paste("u_", resType,sep=""),
           calUnit,
           ufrs,
           msg)


  return(ccc)
}
