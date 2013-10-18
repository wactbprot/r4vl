ce3.calDeltaVDeltat <- function(ccc){
    msg <- "calculated by ce3.calDeltaVDeltat"

    tUnit   <- "s"
    vUnit   <- "l"
    a       <-  abbrevList(ccc)

    if(a$cmscok == "opK1" |a$cmscok == "opK2"|a$cmscok == "opK3"){
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
            stype      <- paste("slope_x_",i, sep="")
            mttype     <- paste("mean_t_", i, sep="")
            ttype      <- paste("t_N_",    i, sep="")
            turntype   <- paste("turn_",   i, sep="")
            mptype     <- paste("mean_p_", i, sep="")

            ## delta t
            mp         <- getConstVal(a$cm, mptype)

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
            mt         <- mt  - min(mt)
            ## ------------------------------------##
            corrSlope  <- slope  - drift[j]
            ci         <- mp     - corrSlope *  mt
            t0         <- (mean(mp) - ci) / corrSlope
            nt         <- length(t0)
            deltat     <- diff(t0) * tconv
            ## ------------------------------------##

            ## Güte des SZ: Steigung mp ~ mt möglichst klein
            mts        <- mt * tconv
            gslope[j]  <- as.numeric(lm(mp ~ mts)$coefficients[2])

            ## delta V
            ## fn.Afit:
            ##
            ## f(x) = ax^3/3+a*b*x^2+x*(a*b^2+c)
            ##
            ## ist definiert in /map/_attachments/R/utils
            ##
            ## A(f(x[2] - f(x1))/(x[2] - x[1]

            ## ------------------------------------##
            h          <- abs(getConstVal(a$cm, turntype)) * t2mm
            nv         <- length(h)
            i1         <- 1:(nv-1)
            i2         <- i1 + 1
            A          <- (fn.Afit(cf,h[i2]) - fn.Afit(cf,h[i1]))/(h[i2] - h[i1])
            deltaV     <- A * (h[i2] - h[i1]) * vconv

            ## Leitwert = dV/dt

            L[j]       <- mean(   deltaV / deltat)
            sdL[j]     <- sd(     deltaV / deltat)
            lL[j]      <- length( deltaV / deltat)
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

        ccc$Calibration$Analysis$Values$Conductance <-
            setCcl(ccc$Calibration$Analysis$Values$Conductance,
                   "g_slope",
                   "mbar/ms",
                   gslope,
                   msg)

    } ## a$cmscok == "opK1" |a$cmscok == "opK2"|a$cmscok == "opK3"

    return(ccc)
}
