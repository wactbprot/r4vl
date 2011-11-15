ce3.calPcal <- function(ccc){
  msg <- "calculated by  ce3.calPcal()"
  a <- abbrevList(ccc)

  Tref          <- getConstVal(a$cc,"referenceTemperature")## 296.15 K
  R             <- getConstVal(a$cc,"R") ## in Pa m^3/mol/K
  mbarls2pam3s  <- getConstVal(a$cc,"mbarl/s_2_Pam^3/s")
  Pa2mbar       <- getConstVal(a$cc,"Pa_2_mbar")
  aK2           <- getConstVal(a$cms,"aK2")
  R             <- getConstVal(a$cc,"R") ## in Pa m^3/mol/K
  qpV           <- getConstVal(a$ca, "qpV") ## FD korregiert
  mfp           <- getConstVal(a$ca,"meanFreePath") ## mfp ... mean free path


  if(a$cmscg =="N2")  M <- getConstVal(a$cc,"molWeight_N2")
  if(a$cmscg =="Ar")  M <- getConstVal(a$cc,"molWeight_Ar")

  if(a$cmscok == "opK1" | a$cmscok == "opK2"| a$cmscok == "opK4"){

    r1        <- getConstVal(a$cms,"r1")
    A1        <- r1^2*pi ## in m^2
    K3        <- getConstVal(a$cms,"K3Uhv")
    Tch       <- getConstVal(a$ca, "Tuhv")
    K2        <- (1 + aK2*(2* r1/mfp))

    ## leitwerte in m^3/s 
    c         <- sqrt(8 * R * Tref/(pi * M)) ## m/s 
    C1        <- c/4 * A1 * K2 * K3

    p         <- qpV * mbarls2pam3s/C1 * Pa2mbar ##  [qpVUhv] = mbar l/s [C1] = m^3/s

    if(a$cmscp == "P1"){ K4 <- getConstVal(a$cms,"K4P1")}
    if(a$cmscp == "P2"){ K4 <- getConstVal(a$cms,"K4P2")}
    if(a$cmscp == "P3"){ K4 <- getConstVal(a$cms,"K4P3")}

    pcal <- p * K4

  }

  if(a$cmscok == "opK3"){

    r2          <- getConstVal(a$cms,"r2")
    A2          <- r2^2*pi ## in m^2

    K3          <- getConstVal(a$cms,"K3Xhv")
    Tch         <- getConstVal(a$ca, "Txhv")
    K2          <- (1 + aK2*(2* r2/mfp))
    
    c           <- sqrt(8 * R * Tref/(pi * M)) ## m/s
    C2          <- c/4*A2 * K2 * K3

    ##  [qpVUhv] = mbar l/s [C1] = m^3/s
    pcal        <- qpV * mbarls2pam3s/C2 * Pa2mbar
  }

  ## spätestens hier ist klar, dass auf jeden Fall
  ## bald ein setter geschrieben gehört!
  ## done!
  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           "cal",
           "mbar",
           pcal,
           paste(msg, "gerechnet mit Tref"))

  return(ccc)
}

