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


  
  ccc$Calibration$Analysis$Values$Error <-
    setCcl(ccc$Calibration$Analysis$Values$Error,
           "relative",
           "1",
            pfrsBs/pfrs5 -1,
           paste(msg, "with p_frs5/p_frs_bs -1"))

  
  Tfrs5  <-  getConstVal(a$cav$Temperature,"frs5") + getConvFactor(ccc,"K","C")
  Tfrsbs <- getConstVal(a$cav$Temperature,"frs_bs") + getConvFactor(ccc,"K","C")
  ## therm transp. corr following Takaishi Sensui 
  p1  <- pfrsBs
  p2  <- pfrs5
  
  T1  <- Tfrsbs
  T2  <- Tfrs5
  
  d   <- 0.01 # 1cm
  
  Tm  <- (T1 + T2)/2
  X   <- 0.133 * p2 * d
  Ap  <- 1.2e6
  Bp  <- 1e3
  Cp  <- 14
  A   <- Ap/Tm^2
  B   <- Bp/Tm
  C   <- Cp/Tm^.5

  p2takSens  <- (A*X^2 +B*X + C*X^.5 + (T2/T1)^.5)/(A*X^2 + B*X + C*X^.5 +1)*p1
  dtaksens   <- p1 - p2takSens

    ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           "p_frs5_therm_corr_tak_sens",
           calUnit,
            p2takSens,
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
           dtaksens/p2takSens ,
           paste(msg, "with d_tak_sens / p_frs5_therm_corr_tak_sens"))
  
                                        #
  ## therm. transp. corr following Setina
  ## Metrologia 36,623-626, (1999)
                                        #
  Kb       <- getConstVal(a$cc,"Kb")
  u        <- getConstVal(a$cc,"u")       ## atom mass const in kg
  M        <- getConstVal(a$cc,"molWeight_N2") ## [M] = kg/mol
  Mr       <- M*1000 ## relative Teilchenmasse [Mr]=1
  visc     <- getConstVal(a$cc,"visc_N2")
  pm       <- (p1+p2)/2
                                        #
  ## Equation following Scharipov
  ## Data on internal rarefied gas flows
  ## 1998, J. Phys. Chem Ref. Data, Vol. 27,No 3
                                        #
  mfp <- sqrt(pi)* visc/(2*pm) * sqrt(2* Kb * Tm/(Mr*u)) ## [l] = m
  Kn  <- mfp/d
  A   <- 0.0181
  B   <- 0.229
  C   <- 0.211
  
  p2Setin <- (1+((T2/T1)^.5 - 1)/(A/Kn^2 + B/Kn + C/Kn^.5 + 1))*p1
  dSetin  <- p1 - p2Setin

  
    ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           "p_frs5_therm_corr_setin",
           calUnit,
            p2Setin,
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
           dSetin/p2Setin ,
           paste(msg, "with d_setin / p_frs5_therm_corr_setin"))

  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           "nd",
           calUnit,
           nd,
           paste(msg, "with (pnd-pndoff)*cf, with cf: ", cf))
 
  return(ccc)
}
