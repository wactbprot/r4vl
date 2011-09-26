calDFrs5FrsBs <- function(ccc){
  msg <- "calculated by calDFrs5FrsBs()"
  a <- abbrevList(ccc)
  
  calUnit <- "Pa"


  
  pnd     <- getConstVal(a$cmv, "nd")
  pndoff  <- getConstVal(a$cmv,"nd_offset")
  
  cf      <- getConstVal(a$cms, "rangeConversionFactor")

  nd      <- (pnd-pndoff) * cf

  ## 
  PFRS5  <- getSubList(a$cav, "frs5")
  PFRSBs <- getSubList(a$cav, "frs_bs")

  pfrs5  <- getConstVal(NA,NA,PFRS5) * getConvFactor(ccc,calUnit,PFRS5$Unit)
  pfrsBs <- getConstVal(NA,NA,PFRSBs) * getConvFactor(ccc,calUnit,PFRSBs$Unit)

  pm       <- (pfrs5+pfrsBs)/2
  
  ccc$Calibration$Analysis$Values$Error <-
    setCcl(ccc$Calibration$Analysis$Values$Error,
           "relative",
           "1",
            pfrs5/pfrsBs -1,
           paste(msg, "with p_frs5/p_frs_bs -1"))

  
  Tfrs5  <-  getConstVal(a$cav$Temperature,"frs5") + getConvFactor(ccc,"K","C")
  Tfrsbs <- getConstVal(a$cav$Temperature,"frs_bs") + getConvFactor(ccc,"K","C")
  ## therm transp. corr following Takaishi Sensui 
  
  T1  <- Tfrsbs
  T2  <- Tfrs5
  
  d   <- 0.016 # 16CF Wellschlauch + 6mm Rohrleitung 
  
  Tm  <- (T1 + T2)/2
  X   <- 0.133 * pfrs5 * d
  Ap  <- 1.2e6
  Bp  <- 1e3
  Cp  <- 14
  A   <- Ap/Tm^2
  B   <- Bp/Tm
  C   <- Cp/Tm^.5

  pfrs5TakSens  <- pfrs5/((A*X^2 +B*X + C*X^.5 + (T2/T1)^.5)/(A*X^2 + B*X + C*X^.5 +1))
  dtaksens      <- pfrs5TakSens - pfrsBs  
  dtaksensnd    <- dtaksens - nd
    ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           "frs5_therm_corr_tak_sens",
           calUnit,
           pfrs5TakSens,
           paste(msg, "with Takaishi Sensui"))
  
  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           "d_tak_sens",
           calUnit,
           dtaksens,
           paste(msg, "with p_frs_bs - p_frs_bln_tak_sens"))
   
  ccc$Calibration$Analysis$Values$Error <-
    setCcl(ccc$Calibration$Analysis$Values$Error,
           "relative_tak_sens",
           "1",
           dtaksens/pfrs5TakSens ,
           paste(msg, "with d_tak_sens / p_frs5_therm_corr_tak_sens"))

  ccc$Calibration$Analysis$Values$Error <-
    setCcl(ccc$Calibration$Analysis$Values$Error,
           "relative_tak_sens_nd",
           "1",
           dtaksensnd/pfrs5TakSens ,
           paste(msg, "with (d_tak_sens - nd) / p_frs5_therm_corr_tak_sens"))
  
                                        #
  ## therm. transp. corr following Setina
  ## Metrologia 36,623-626, (1999)
  Kb       <- getConstVal(a$cc,"Kb")
  u        <- getConstVal(a$cc,"u")            ## atom mass const in kg
  M        <- getConstVal(a$cc,"molWeight_N2") ## [M] = kg/mol
  Mr       <- M*1000                           ## relative Teilchenmasse [Mr]=1
  visc     <- getConstVal(a$cc,"visc_N2")      ## viskosität
  ## Equation following Scharipov
  ## Data on internal rarefied gas flows
  ## 1998, J. Phys. Chem Ref. Data, Vol. 27,No 3
                                        #
  mfp <- sqrt(pi)* visc/(2*pm) * sqrt(2* Kb * Tm/(Mr*u)) ## [l] = m
  Kn  <- mfp/d
  A   <- 0.0181
  B   <- 0.229
  C   <- 0.211
  
  pfrs5Setin <- pfrs5/(1+((T2/T1)^.5 - 1)/(A/Kn^2 + B/Kn + C/Kn^.5 + 1))
  dSetin     <- pfrs5Setin - pfrsBs

  
    ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           "frs5_therm_corr_setin",
           calUnit,
           pfrs5Setin,
           paste(msg, "with Setina"))
  
  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           "d_setin",
           calUnit,
           dSetin,
           paste(msg, "with p_frs_bs - p_frs_bln_setin"))
  
  ccc$Calibration$Analysis$Values$Error <-
    setCcl(ccc$Calibration$Analysis$Values$Error,
           "relative_setin",
           "1",
           dSetin/pm,
           paste(msg, "with d_setin / p"))

  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           "nd",
           calUnit,
           nd,
           paste(msg, "with (pnd-pndoff)*cf, with cf: ", cf))
 
  return(ccc)
}
