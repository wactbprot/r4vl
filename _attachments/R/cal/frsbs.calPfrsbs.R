frsbs.calPfrsbs <- function(ccc){
  msg <- "calculated by frsbs.calPfrsbs()"
  a <- abbrevList(ccc)
  
  TUnit   <- "C"
  RUnit   <- "lb"
  calUnit <- "Pa"
  resType <- "frs_bs"
  
  G       <-  getSubList( a$cc,    "g")      ## in m/s^2 <- nochmal überdenken, g in Berlin!?
  TFRS    <-  getSubList( a$cmv$Temperature, "keithley_ch202")
  ##  Konstanten kommen _nicht_
  ##  aus dem Standard sondern aus dem 1. CalibrierObject
  RCAL    <-  getSubList( a$cmco1,    "R_cal")          ## in lb
  MCAL    <-  getSubList( a$cmco1,    "m_cal")          ## in in kg
  AEFF    <-  getSubList( a$cmco1,    "A_eff")          ## in in m^2
  RHOFrs  <-  getSubList( a$cmco1,    "rho_frs")        ## in in kg/m^3
  RHOGas  <-  getSubList( a$cmco1,    "rho_gas")        ## in in kg/m^3
  AB      <-  getSubList( a$cmco1,    "alpha_beta_frs") ## in in kg/m^3
  
  ## check der Einheiten
  g       <-  getConstVal(NA,NA,G)      * getConvFactor(ccc,"m/s^2",G$Unit)
  
  Rcal    <-  getConstVal(NA,NA,RCAL)   * getConvFactor(ccc,RUnit,RCAL$Unit)
  mcal    <-  getConstVal(NA,NA,MCAL)   * getConvFactor(ccc,"kg",MCAL$Unit)
  Aeff    <-  getConstVal(NA,NA,AEFF)   * getConvFactor(ccc,"m^2",AEFF$Unit)
  
  rhoFrs  <-  getConstVal(NA,NA,RHOFrs) * getConvFactor(ccc,"kg/m^3",RHOFrs$Unit)
  rhoGas  <-  getConstVal(NA,NA,RHOGas) * getConvFactor(ccc,"kg/m^3",RHOGas$Unit)
  
  alphaBeta  <- getConstVal(NA,NA,AB)   * getConvFactor(ccc,"1/C",AB$Unit)
  
  Tfrs       <- getConstVal(NA,NA,TFRS) *  getConvFactor(ccc,TUnit,TFRS$Unit)  # C
  
  RFRS       <- getSubList(a$cmv,    "frs_bs")        ## in lb
  RFRSOFF    <- getSubList(a$cmv,    "frs_bs_offset") ## in lb
  
  Rfrs       <- getConstVal(NA,NA,RFRS) * getConvFactor(ccc,RUnit,RFRS$Unit)
  Rfrsoff    <- getConstVal(NA,NA,RFRSOFF) * getConvFactor(ccc,RUnit,RFRSOFF$Unit)
  
  rhoCorr       <- (1-rhoGas/rhoFrs)
  alphaBetaCorr <- (1 + alphaBeta*(Tfrs - 20))
  
  ## ---------vvv--------Restgas--------
  
  PRes       <- getSubList(a$cmv,    "frs_bs_res") ## in mbar
  ##PResOff    <- getSubList(a$cmv,    "frs_bs_res_offset") ## in mbar
  
  Pres       <- getConstVal(NA,NA,PRes) * getConvFactor(ccc,calUnit,PRes$Unit)
  
                                        #
  ##  Presoff    <- getConstVal(NA,NA,PResOff) * getConvFactor(ccc,calUnit,PResOff$Unit)  
  ## "-  Presoff" weg! -> eine wirkliche Offsetmessung gibt es nicht
  ## es sollten nur eine entsprechende unsicherheit angenommen werden
                                        #
  pres     <-  Pres
  ## ---------^^^----------------
  
  R         <- Rfrs - Rfrsoff
  
  pfrs <- R/Rcal * mcal * g/Aeff * rhoCorr * alphaBetaCorr + pres ## liefert Pa
  
  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           resType,
           calUnit,
           pfrs,
           msg)
  
  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           paste(resType,"_res",sep=""),
           calUnit,
           pres,
           msg)

  return(ccc)
}
