frs5.calPfrs5 <- function(ccc){
  msg <- "calculated by frs5.calPfrs5()"
  a <- abbrevList(ccc)

  TUnit   <- "C"
  RUnit   <- "lb"
  calUnit <- "Pa"
  resType <- "cal"
  ## aber:
  if(a$ct == "VG" |  a$ct == "IK"){
    resType <- "frs5"
  }
  ## --   R.cal <- 2.290320
  ## --   m.cal <- 1038.867
  ## --   g.cal <- 9.812718
  ## --   A.0 <- 45.3467
  ## --   rho.gas <- 0
  ## --   rho.masse <- 7920
  ## --   rho.corr <- (1-rho.gas/rho.masse)
  ## --   theta.corr <- (1+4e-7*(theta-20))
  ## --
  ## --   mult <- m.cal*g.cal*rho.corr/R.cal/A.0/theta.corr/10

  G      <- getSubList(a$cc,    "g")      ## in m/s^2

  TFRS   <- getSubList(a$cmv$Temperature, "keithley_ch203")

  RCAL   <- getSubList(a$cms$Constants,    "R_cal") ## in lb
     
  MCAL   <- getSubList(a$cms$Constants,    "m_cal") ## in in kg

  AEFF   <- getSubList(a$cms$Constants,    "A_eff") ## in in m^2

  RHOFrs <-  getSubList(a$cms$Constants,    "rho_frs") ## in in kg/m^3
  RHOGas <-  getSubList(a$cms$Constants,    "rho_gas") ## in in kg/m^3
  AB     <-  getSubList(a$cms$Constants,    "alpha_beta_frs") ## in in kg/m^3
  ## check der Einheiten
  g      <- getConstVal(NA,NA,G) * getConvFactor(ccc,"m/s^2",G$Unit)

  Rcal   <- getConstVal(NA,NA,RCAL) * getConvFactor(ccc,RUnit,RCAL$Unit)
  mcal   <- getConstVal(NA,NA,MCAL) * getConvFactor(ccc,"kg",MCAL$Unit)
  Aeff   <- getConstVal(NA,NA,AEFF) * getConvFactor(ccc,"m^2",AEFF$Unit)

  rhoFrs <- getConstVal(NA,NA,RHOFrs) * getConvFactor(ccc,"kg/m^3",RHOFrs$Unit)
  rhoGas <- getConstVal(NA,NA,RHOGas) * getConvFactor(ccc,"kg/m^3",RHOGas$Unit)

  alphaBeta <- getConstVal(NA,NA,AB) * getConvFactor(ccc,"1/C",AB$Unit)

  Tfrs      <- getConstVal(NA,NA,TFRS)    *  getConvFactor(ccc,TUnit,TFRS$Unit)  # C

  RFRS    <- getSubList(a$cmv,    "frs_p") ## in lb
  RFRSZc  <- getSubList(a$cmv,    "frs_zc_p") ## in lb
  RFRSZc0 <- getSubList(a$cma,    "frs_zc0_p") ## in lb

  Rfrs      <- getConstVal(NA,NA,RFRS) * getConvFactor(ccc,RUnit,RFRS$Unit)
  RfrsZc    <- getConstVal(NA,NA,RFRSZc) * getConvFactor(ccc,RUnit,RFRSZc$Unit)
  RfrsZc0   <- getConstVal(NA,NA,RFRSZc0) * getConvFactor(ccc,RUnit,RFRSZc0$Unit)

  rhoCorr <- (1-rhoGas/rhoFrs)
  alphaBetaCorr<- (1 + alphaBeta*(Tfrs - 20))

  ## ---------vvv--------ToDo--------
  ## s. Ticket #79
  ## pres
  ## das srg- Problem löse ich später
  ## hier reicht Faktor 27 an die dcr- Werte zu multiplizieren
  srgD        <- getConstVal(a$cmco,  "d")              ## in m
  srgRho      <- getConstVal(a$cmco,  "rho")            ## in kg/m^3
  srgSigma    <- getConstVal(a$cmco,  "sigma_eff_N2")   ##
  molWeightN2 <- getConstVal(a$cc,    "molWeight_N2")   ## in kg/mol
  srgR        <- getConstVal(a$cc,    "R")              ## in Pa m^3/mol/K

  Tsrg        <- Tfrs + getConvFactor(ccc,"K",TFRS$Unit)
  
  srgK        <- (8*srgR*Tsrg/(pi*molWeightN2))^0.5*pi*srgD*srgRho/20
  
  pdcrOff     <- getConstVal(a$cma,"frs_res_off")
  pdcr        <- getConstVal(a$cmv,"frs_res")

  pres <- (pdcr - pdcrOff) * srgK / srgSigma ##  ~Umrechnung dcr in Pa bei 23C und N2
  ## ---------^^^--------ToDo--------


  R0        <- RfrsZc - RfrsZc
  R         <- Rfrs - R0

  pfrs <- R/Rcal*mcal*g/Aeff*rhoCorr*alphaBetaCorr + pres ## liefert Pa


 if(length(a$cmscoi) > 0){
   pfrs <- checkOutIndex(a,pfrs)
   pres <- checkOutIndex(a,pres)
   msg <- paste(msg, "skiped points:", a$cmscoi, "see function getOutIndex() why")
 }

  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           resType,
           calUnit,
           pfrs,
           msg)


  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           "frs5_res",
           calUnit,
           pres,
           paste(msg,"constants:",srgD, srgRho,srgSigma,molWeightN2,srgR,Tsrg, srgK))
 

  return(ccc)
}
