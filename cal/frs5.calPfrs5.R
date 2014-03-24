frs5.calPfrs5 <- function(ccc){
  msg <- "calculated by frs5.calPfrs5()"
  a <- abbrevList(ccc)

  TUnit   <- "C"
  RUnit   <- "lb"
  calUnit <- "mbar"
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

  G         <- getSubList(a$cc,    "g")      ## in m/s^2

  TFRS      <- getSubList(a$cav$Temperature, "frs5")
  Tfrs      <- getConstVal(NA, NA, TFRS)
  RCAL      <- getSubList(a$cms,   "R_cal") ## in lb
  MCAL      <- getSubList(a$cms,   "m_cal") ## in in kg
  AEFF      <- getSubList(a$cms,   "A_eff") ## in in m^2
  RHOFrs    <- getSubList(a$cms,   "rho_frs") ## in in kg/m^3
  RHOGas    <- getSubList(a$cms,   "rho_gas") ## in in kg/m^3
  AB        <- getSubList(a$cms,   "alpha_beta_frs") ## in in kg/m^3
  ## check der Einheiten
  g         <- getConstVal(NA,NA,G)      * getConvFactor(ccc,"m/s^2",G$Unit)
  Rcal      <- getConstVal(NA,NA,RCAL)   * getConvFactor(ccc,RUnit,RCAL$Unit)
  mcal      <- getConstVal(NA,NA,MCAL)   * getConvFactor(ccc,"kg",MCAL$Unit)
  Aeff      <- getConstVal(NA,NA,AEFF)   * getConvFactor(ccc,"m^2",AEFF$Unit)

  rhoFrs    <- getConstVal(NA,NA,RHOFrs) * getConvFactor(ccc,"kg/m^3",RHOFrs$Unit)
  rhoGas    <- getConstVal(NA,NA,RHOGas) * getConvFactor(ccc,"kg/m^3",RHOGas$Unit)
  alphbet   <- getConstVal(NA,NA,AB)     * getConvFactor(ccc,"1/C",AB$Unit)

  RFRS      <- getSubList(a$cmv,    "frs_p") ## in lb
  Rfrs      <- getConstVal(NA,NA,RFRS) * getConvFactor(ccc,RUnit,RFRS$Unit)
  
  RFRSZc    <- getSubList(a$cmv,    "frs_zc_p") ## in lb
  RfrsZc    <- getConstVal(NA,NA,RFRSZc) * getConvFactor(ccc,RUnit,RFRSZc$Unit)

  RFRSZc0   <- getSubList(a$cma,    "frs_zc0_p") ## in lb
  rfrsZc0   <- getConstVal(NA,NA,RFRSZc0) * getConvFactor(ccc,RUnit,RFRSZc0$Unit)

  ## initialaer ZC kann mehrmals gemessen werden
  ## über die Zeiten amt_(absolute measure time)_offset
  ## wird der gültige ermittelt
  omt       <- getConstVal(a$cma, "amt_offset")
  bmt       <- getConstVal(a$cmv, "amt_before")
  N         <- length(bmt)
  
  if(!is.null(omt) & !is.null(bmt) & 
     (length(omt) > 1) & (length(bmt) > 1)){
        RfrsZc0 <- rep(NA,N)
        for(i in 1:length(omt)){
          k         <- which(bmt > omt[i])
          RfrsZc0[k] <- rfrsZc0[i] 
      }
  }else{
      RfrsZc0 <- rep(rfrsZc0, N)
  }
  
  rhoCorr <- (1-rhoGas/rhoFrs)
  alphbetCorr<- (1 + alphbet*(Tfrs - 20))
  
  
  srgD        <- getConstVal(a$cmco,  "d")              ## in m
  srgRho      <- getConstVal(a$cmco,  "rho")            ## in kg/m^3
  srgSigma    <- getConstVal(a$cmco,  "sigma_eff_N2")   ##
  molWeightN2 <- getConstVal(a$cc,    "molWeight_N2")   ## in kg/mol
  srgR        <- getConstVal(a$cc,    "R")              ## in Pa m^3/mol/K

  Tsrg        <- Tfrs + getConvFactor(ccc,"K",TFRS$Unit)
  
  srgK        <- (8*srgR*Tsrg/(pi*molWeightN2))^0.5*pi*srgD*srgRho/2000 #mbar
  
  pdcrOff     <- getConstVal(a$cma,"frs_res_off")
  pdcr        <- getConstVal(a$cmv,"frs_res")

  pres <- (pdcr - pdcrOff) * srgK / srgSigma ##  ~Umrechnung dcr in mbar bei 23C und N2
  ## ---------^^^--------ToDo--------


  R0        <- RfrsZc - RfrsZc0
  R         <- Rfrs - R0

  pfrs      <- R/Rcal*mcal*g/Aeff*rhoCorr*alphbetCorr + pres ## liefert Pa

  conv      <- getConvFactor(ccc,calUnit,"Pa")

  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           resType,
           calUnit,
           pfrs * conv,
           msg)


  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           "frs5_res",
           calUnit,
           pres,
           paste(msg,"constants (srgD, srgRho, srgSigma, molWeightN2, srgR, Tsrg, srgK):",
                 srgD ,",", srgRho,",", srgSigma,",", molWeightN2, ",", srgR, ",",Tsrg,",", srgK))
  
  return(ccc)
}
