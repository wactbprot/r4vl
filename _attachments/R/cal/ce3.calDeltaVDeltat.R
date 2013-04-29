ce3.calDeltaVDeltat <- function(ccc){
  msg <- "calculated by ce3.calDeltaVDeltat"

  tUnit   <- "s"
  vUnit   <- "l"
  a       <-  abbrevList(ccc)
  DRIFT   <-  getSubList(a$cmv,"drift_slope_x")
  drift   <-  getConstVal(NA,NA,DRIFT)

  LWSTART   <-  getSubList(a$cma,"start_lw")
  lwstart   <-  getConstVal(NA,NA,LWSTART)

  pfill     <- getConstVal(a$cav, "fill")
  
  cf      <- list()
  cf$A    <-  getConstVal(a$cms, "fbv_A")
  cf$B    <-  getConstVal(a$cms, "fbv_B")
  cf$C    <-  getConstVal(a$cms, "fbv_C")

  L       <-  NULL
  sdL     <-  NULL
  lL      <-  NULL
  gamma   <-  NULL
  Mp      <-  NULL
  meanMp  <-  NULL
  sdMeanMp<-  NULL
  gslope  <-  NULL
  Vol     <-  NULL
  corrF   <-  NULL
  dt      <-  NULL
  t2mm    <- getConstVal(a$cms,"turn_2_mm")
  ms2s    <- getConstVal(a$cc,"ms_2_s")
  vconv   <- getConvFactor(ccc,vUnit, "mm^3")
  j       <- 0
  noOfSZ  <-  length(lwstart)

  ## nicht nach p=0 extrap.
  ## sondern t bei p_mean
  ## ermitteln weil
  ## sonst der Hebel
  ## der Extrapolation und damit die 
  ## Streuung zu groß  wird
 
  for(i in lwstart){
    j <- j+1
    stype      <- paste("slope_x_",i,sep="")
    mttype     <- paste("mean_t_", i,sep="")
    ttype      <- paste("t_N_",    i,sep="")
    turntype   <- paste("turn_",   i,sep="")
    mt         <- paste("mean_p_", i,sep="")

    ## delta t
    mp         <- getConstVal(a$cm, mt)    
    MT         <- getSubList(a$cm, mttype)
    mt         <- getConstVal(NA,NA,MT)
    TE         <- getSubList(a$cm, ttype)
    te         <- getConstVal(NA,NA,TE)
    SLOPE      <- getSubList(a$cm,stype)
    slope      <- getConstVal(NA,NA,SLOPE)
    tconv      <- getConvFactor(ccc,tUnit, MT$Unit)
    ##
    ## die Extrapolation erfolgt zu mean(mp)
    ## das ist der mittelwert der mittleren
    ## sz-Drücke; die "Extrapolationslänge"
    ## wird so minimal.
    mt <- mt  - min(mt)
    ## ------------------------------------##
    corrSlope  <- slope  - drift[j]
    ci         <- mp     - corrSlope *  mt
    t0         <- (mean(mp) - ci) / corrSlope
    dp.corr    <- diff(mp)
    ## ------------------------------------##

    ## Güte des SZ: Steigung mp ~ mt möglichst klein
    gslope[j]  <- as.numeric(lm(mp ~ mt)$coefficients[2])
    
    nt         <- length(t0)
    j1         <- 1:(nt-1)
    j2         <- j1 + 1
    
    deltat     <- (t0[j2] - t0[j1]) * tconv 
    ## delta V
    h          <- abs(getConstVal(a$cm, turntype)) * t2mm
    nv         <- length(h)
    i1         <- 1:(nv-1)
    i2         <- i1 + 1
    
    ## fn.Afit:
    ##
    ## f(x) = ax^3/3+a*b*x^2+x*(a*b^2+c)
    ##
    ## ist definiert in /map/_attachments/R/utils
    ##
    ## A(f(x[2] - f(x1))/(x[2] - x[1]
    
    ## ------------------------------------##   
    A          <- (fn.Afit(cf,h[i2]) - fn.Afit(cf,h[i1]))/(h[i2] - h[i1])
    deltaV     <- A * (h[i2] - h[i1]) * vconv
    ## Leitwert = dV/dt
    Vol[j]     <- mean(deltaV)
    dt[j]      <- mean(deltat)
    

    corrF[j]   <- mean(dp.corr/pfill[j]*A * vconv/deltat) *nt

    
    L[j]       <- mean(deltaV / deltat)
    sdL[j]     <- sd(deltaV / deltat)
    lL[j]      <- length(deltaV / deltat)
    ## ------------------------------------##

  }

  lw2List    <- getSubList(a$cms, "useLw2")
  
  
  ccc$Calibration$Analysis$Values$Conductance <-
    setCcl(ccc$Calibration$Analysis$Values$Conductance,
           "cnom",
           "l/s",
           L,
           msg)

  ilw    <- getConductIndex(ccc)
  
  corrConst <- rep(NA, length(L))

  if(length(ilw$iLw2) > 0){
    corrConst[ilw$iLw2] <- 0.0004017
  }
  
  if(length(ilw$iLw1) > 0){
    corrConst[ilw$iLw1] <-  0.001525
  }

  print(corrF / corrConst/Vol)
 
  
  ccc$Calibration$Analysis$Values$Conductance <-
    setCcl(ccc$Calibration$Analysis$Values$Conductance,
           "sd_cnom",
           "l/s",
           sdL,
           msg)

  ccc$Calibration$Analysis$Values$Conductance <-
    setCcl(ccc$Calibration$Analysis$Values$Conductance,
           "N_cnom",
           "l/s",
           lL,
           msg)

  ccc$Calibration$Analysis$Values$Conductance <-
    setCcl(ccc$Calibration$Analysis$Values$Conductance,
           "g_slope",
           "mbar/ms",
           gslope,
           msg)

   return(ccc)
}
