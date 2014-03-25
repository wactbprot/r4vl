ce3.calDeltaVDeltat <- function(ccc){
    msg <- "calculated by ce3.calDeltaVDeltat"

    tUnit   <- "s"
    vUnit   <- "l"
    a       <-  abbrevList(ccc)

    PFILL   <- getSubList(a$cav, "fill")
    pfill   <- getConstVal(NA, NA, PFILL)
    
    
    if(a$cmscok == "opK1" |a$cmscok == "opK2"|a$cmscok == "opK3"){
        L       <-  NULL
        sdL     <-  NULL
        lL      <-  NULL
        gamma   <-  NULL
        Mp      <-  NULL
        meanMp  <-  NULL
        sdMeanMp<-  NULL
        gSlope  <-  NULL
        minSlope<-  NULL
        maxSlope<-  NULL
        mSlope  <-  NULL
        dcorr   <-  NULL
        
        DRIFT   <-  getSubList(a$cmv,"drift_slope_x")
        drift   <-  getConstVal(NA,NA,DRIFT)

        LWSTART <-  getSubList(a$cma,"start_lw")
        lwstart <-  getConstVal(NA,NA,LWSTART)

        cf      <- list()
        cf$A    <-  getConstVal(a$cms, "fbv_A")
        cf$B    <-  getConstVal(a$cms, "fbv_B")
        cf$C    <-  getConstVal(a$cms, "fbv_C")


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
            ## ------------------------------------##
            mt         <- mt  - min(mt)
            ci         <- mp - slope *  mt
            t0         <- (mean(mp) - ci) / slope 
            nt         <- length(t0)
            deltat     <- diff(t0) * tconv
            ## ------------------------------------##

            ## Güte des SZ: Steigung mp ~ mt möglichst klein
            gSlope[j]  <- as.numeric(lm(mp ~ mt)$coefficients[2])
            mSlope[j]  <- mean(slope)
            minSlope[j]<- min(slope)
            maxSlope[j]<- max(slope)
            ## s. http://a73434.berlin.ptb.de/mediawiki/index.php
            dcorr[j]   <- drift[j]/mSlope[j] 
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

            ## Leitwert = dV/dt *(1- m.D/m.SZ)
            dVdt       <- deltaV / deltat * (1 - dcorr[j])
            L[j]       <- mean(  dVdt )
            sdL[j]     <- sd(    dVdt )
            lL[j]      <- length(dVdt )
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
                   gSlope)
        
        ccc$Calibration$Analysis$Values$Conductance <-
            setCcl(ccc$Calibration$Analysis$Values$Conductance,
                   "min_slope",
                   "mbar/ms",
                   minSlope)
        
        ccc$Calibration$Analysis$Values$Conductance <-
            setCcl(ccc$Calibration$Analysis$Values$Conductance,
                   "max_slope",
                   "mbar/ms",
                   maxSlope)
        
        ccc$Calibration$Analysis$Values$Conductance <-
            setCcl(ccc$Calibration$Analysis$Values$Conductance,
                   "mean_slope",
                   "mbar/ms",
                   mSlope)

        ccc$Calibration$Analysis$Values$Conductance <-
            setCcl(ccc$Calibration$Analysis$Values$Conductance,
                   "drift_slope",
                   "mbar/ms",
                   drift)

        ccc$Calibration$Analysis$Values$Conductance <-
            setCcl(ccc$Calibration$Analysis$Values$Conductance,
                   "drift_corr",
                   "1",
                   dcorr)
        
    } ## a$cmscok == "opK1" |a$cmscok == "opK2"|a$cmscok == "opK3"

    if(a$cmscok == "opK4"){
        if(PFILL$Unit == "mbar"){
            
            dv2MolCIntercept <-  getConstVal(a$cms$Constants, "dv2MolCIntercept")
            dv2MolCSlope  <-  getConstVal(a$cms$Constants, "dv2MolCSlope") 
                        
            ccc$Calibration$Analysis$Values$Conductance <-
                setCcl(ccc$Calibration$Analysis$Values$Conductance,
                       "cnom",
                       "l/s",
                       dv2MolCSlope * pfill + dv2MolCIntercept,
                   msg)
        }
    }
    return(ccc)
}
