ce3.calDeltaVDeltat <- function(ccc){
  msg <- "calculated by ce3.calDeltaVDeltat"

  tUnit   <- "s"
  vUnit   <- "l"
  a       <-  abbrevList(ccc)
  DRIFT   <-  getSubList(a$cm,"drift_slope_x")
  drift   <-  getConstVal(NA,NA,DRIFT)

  cf      <- list()
  cf$A    <-  getConstVal(a$cms, "fbv_A")
  cf$B    <-  getConstVal(a$cms, "fbv_B")
  cf$C    <-  getConstVal(a$cms, "fbv_C")

  L       <-  NULL
  sdL     <-  NULL
  lL      <-  NULL
  Mp      <-  NULL
  meanMp  <-  NULL
  sdMeanMp<-  NULL

  t2mm    <- getConstVal(a$cms,"turn_2_mm")
  ms2s    <- getConstVal(a$cc,"ms_2_s")
  vconv   <- getConvFactor(ccc,vUnit, "mm^3")

  noOfSZ  <-  length(drift)

  ## nicht nach p=0 extrap.
  ## sondern t bei p_mean
  ## ermitteln ...
    for(k in 0:(noOfSZ - 1)){
    mt         <- paste("mean_p_", k,sep="")
    mp         <- getConstVal(a$cm, mt)
    Mp         <- append(Mp, mean(mp))
  }
  ## ... sonst wird der Hebel
  ## der extrapolation zu groß
  ## und damit die Streuung
  pMean <- mean(Mp)
 
  for(i in 0:(noOfSZ - 1)){

    stype      <- paste("slope_x_",i,sep="")
    mptype     <- paste("mean_p_", i,sep="")
    mttype     <- paste("mean_t_", i,sep="")
    ttype      <- paste("t_N_",    i,sep="")
    turntype   <- paste("turn_",   i,sep="")

    ## delta t 
    mp         <- getConstVal(a$cm, mptype)
    meanMp     <- append(meanMp, mean(mp))
    sdMeanMp   <- append(sdMeanMp, sd(mp))
    MT         <- getSubList(a$cm, mttype)
    mt         <- getConstVal(NA,NA,MT)
    TE         <- getSubList(a$cm, ttype)
    te         <- getConstVal(NA,NA,TE)
    SLOPE      <- getSubList(a$cm,stype)
    slope      <- getConstVal(NA,NA,SLOPE)
    tconv      <- getConvFactor(ccc,tUnit, MT$Unit)
    
    ## ------------------------------------##
    corrSlope  <- slope - drift[i+1]
    ci         <- mp -  corrSlope *  mt
    t0         <- (pMean-ci)/corrSlope
    ## ------------------------------------##
    
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
    L          <- append(L,    mean(deltaV / deltat))
    sdL        <- append(sdL,    sd(deltaV / deltat))
    lL         <- append(lL, length(deltaV / deltat))
    ## ------------------------------------##

  }

  
  ccc$Calibration$Analysis$Values$Conductance <-
    setCcl(ccc$Calibration$Analysis$Values$Conductance,
           "cnom",
           "l/s",
           L,
           msg)

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

  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           "lw_mea",
           "mbar",
           meanMp,
           msg)

  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           "sd_lw_mea",
           "mbar",
           sdMeanMp,
           msg)

   return(ccc)
}
