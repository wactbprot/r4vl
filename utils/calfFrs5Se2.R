calfFrs5Se2 <- function(ccc){
    msg <- "calculated by calDFrs5FrsBs()"
    a <- abbrevList(ccc)

    calUnit <- "mbar"

    pnd     <- getConstVal(a$cmv, "nd")
    pndoff  <- getConstVal(a$cmv, "nd_offset")
    cf      <- getConstVal(a$cmco, "rangeConversionFactor")
    
    nd.Torr <- (pnd - pndoff) * cf ## liefert Torr
    nd      <- nd.Torr * getConvFactor(ccc, calUnit, "Torr")


    Tbefore <- getConstVal(a$cav$Temperature, "before")
    Tafter  <- getConstVal(a$cav$Temperature, "after")
    rg      <- getConstVal(a$cav$Correction,  "rg")

    PFRS    <- getSubList(a$cav$Pressure, "frs5")
    pfrs    <- getConstVal(NA, NA, PFRS) * getConvFactor(ccc,calUnit,PFRS$Unit)

    PFILL   <- getSubList(a$cav, "fill")
    pfill   <- getConstVal(NA,NA,PFILL) * getConvFactor(ccc,calUnit,PFILL$Unit)

    N       <- length(pfill)

    exName  <- getSubList(a$cmv$Expansion, "name")$Value
    if1     <- which(exName == "f1")
    if2     <- which(exName == "f2")
    if3     <- which(exName == "f3")
    if4     <- which(exName == "f4")
    if5     <- which(exName == "f5")

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
    print(rg)
    f.prime <- (pfrs - nd) * Tbefore/((1 - rg) * pfill * Tafter)
    f.pure  <- 1/(1/f.prime - V.add/V.start)
    
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
