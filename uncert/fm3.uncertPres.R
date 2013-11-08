fm3.uncertPres <- function(ccc){
    msg <- "Calculated by fm3.uncertPres()"
    
    a   <- abbrevList(ccc)
    
    PFILL     <- getSubList(a$cav, "fill")
    pfill     <- getConstVal(NA, NA, PFILL)

    UPRES     <- getSubList(a$cms, "fm3Pres_u1")
    upres      <- getConstVal(NA, NA, UPRES)

    uncertRes <- rep(1, length(pfill))
                     
    if(PFILL$Unit == UPRES$Unit){
        uncertRes <- upres/pfill
    }
    
    ccc$Calibration$Analysis$Values$Uncertainty <-
        setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
               "uncertPres",
               "1",
               uncertRes,
               msg)


    return(ccc)
}
