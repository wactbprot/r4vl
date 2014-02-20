calfFrs5Se2 <- function(ccc){
    msg <- "calculated by calDFrs5FrsBs()"
    a <- abbrevList(ccc)

    calUnit <- "mbar"

    ## Nullindicator
    convFact <- getConstVal(a$cmco, "rangeConversionFactor")
    unitConf <- getConvFactor(ccc, calUnit, "Torr")
    pnd      <- getConstVal(a$cmv, "nd") * convFact * unitConf
    pndoff   <- getConstVal(a$cmv, "nd_offset") * convFact * unitConf

    nd      <- pnd - pndoff

    ## Temperatur
    Tbefore <- getConstVal(a$cav$Temperature, "before")
    Tafter  <- getConstVal(a$cav$Temperature, "after")
    
    rg      <- getConstVal(a$cav$Correction,  "rg")

                                        # Druck FRS
    PFRS    <- getSubList(a$cav$Pressure, "frs5")
    pfrs    <- getConstVal(NA, NA, PFRS) *
        getConvFactor(ccc,calUnit,PFRS$Unit)

    ## Druck Ruska
    PFILL   <- getSubList(a$cav, "fill")
    pfill   <- getConstVal(NA,NA,PFILL) *
        getConvFactor(ccc,calUnit,PFILL$Unit)

    ## Expansion
    N       <- length(pfill)
    exName  <- getSubList(a$cmv$Expansion, "name")$Value
    if1     <- which(exName == "f1")
    if2     <- which(exName == "f2")
    if3     <- which(exName == "f3")
    if4     <- which(exName == "f4")
    if5     <- which(exName == "f5")

    ## Startvolumen
    V.start <- rep(NA,N)
    V.add   <- getConstVal(a$cav$Volume, "additional")

    if(length(if1) > 0){
        V.start[if1] <- getConstVal(a$cms, "V1")
    }
    if(length(if2) > 0){
        V.start[if2] <- getConstVal(a$cms, "V2")
    }
    if(length(if3) > 0){
        V.start[if3] <- getConstVal(a$cms, "V1")
    }
    if(length(if4) > 0){
        V.start[if4] <- getConstVal(a$cms, "V2")
    }
    if(length(if5) > 0){
        V.start[if5] <- getConstVal(a$cms, "V5")
    }


    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    f.prime <- (pfrs - nd) * Tbefore/((1 - rg) * pfill * Tafter)
    f.pure  <- 1/(1/f.prime - V.add/V.start)
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ccc$Calibration$Analysis$Values$Pressure <-
        setCcl(ccc$Calibration$Analysis$Values$Pressure,
               "nd_corr",
               calUnit,
               nd,
               msg)
    ccc$Calibration$Analysis$Values$Pressure <-
        setCcl(ccc$Calibration$Analysis$Values$Pressure,
               "nd",
               calUnit,
               pnd,
               msg)
    ccc$Calibration$Analysis$Values$Pressure <-
        setCcl(ccc$Calibration$Analysis$Values$Pressure,
               "nd_offset",
               calUnit,
               pndoff,
               msg)
    ccc$Calibration$Analysis$Values$Expansion <-
        setCcl(ccc$Calibration$Analysis$Values$Expansion,
               "f_pure",
               "1",
               f.pure,
               paste(msg, "um Zusatzvolumen korregierter Wert"))
    ccc$Calibration$Analysis$Values$Expansion <-
        setCcl(ccc$Calibration$Analysis$Values$Expansion,
               "f_prime",
               "1",
               f.prime,
               "Zusatzvolumen noch nicht korregierter")

    return(ccc)
}
