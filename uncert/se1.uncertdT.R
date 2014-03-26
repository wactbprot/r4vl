se1.uncertdT <- function(ccc){
    msg <- "calculated by  se1.uncertdT()"
    a   <- abbrevList(ccc)

    PFILL        <- getSubList(a$cav, "fill")
    pfill        <- getConstVal(NA, NA, PFILL)

    if(PFILL$Unit == "mbar"){

        Tafter       <- getConstVal(a$cav, "after")

        ## Beiträge der Sensor-Kalib.:
        us1 <- getConstVal(a$cmco, "keithley_u1") ## abs.
        us2 <- getConstVal(a$cmco, "keithley_u2") ## abs.
        us3 <- getConstVal(a$cmco, "keithley_u3") ## abs.
        us4 <- getConstVal(a$cmco, "keithley_u4") ## abs.
        us5 <- getConstVal(a$cmco, "keithley_u5") ## abs.
        us6 <- getConstVal(a$cmco, "keithley_u6") ## abs.

        usc <- sqrt(us1^2 + us2^2 + us3^2 + us4^2 + us5^2 + us6^2)

        ## Beiträge des Normals
        un1 <- getConstVal(a$cms, "dT_u1") ## abs.
        un2 <- getConstVal(a$cms, "dT_u2") ## abs.

        udT <- sqrt(un1^2 + usc^2 + (pfill*un2)^2)/Tafter  ## rel.

        ccc$Calibration$Analysis$Values$Uncertainty <-
            setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
                   "uncertdT",
                   "1",
                   udT,
                   msg)
    }
    return(ccc)
}
