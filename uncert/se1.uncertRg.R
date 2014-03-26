se1.uncertRg <- function(ccc){
    msg   <- "calculated by  se1.uncertRg()"
    a     <- abbrevList(ccc)


    rg    <- getConstVal(a$cav, "rg")
    pfill <- getConstVal(a$cav, "fill") # in mbar
    pcal  <- getConstVal(a$cav, "cal") # in mbar
    
    ## --- Realgascorrektur
    uf1  <- getConstVal(a$cms, "F1_u1") ## in 1/mbar
    uF1  <- (1 - rg) * uf1 * pfill ## rel.
    
    ccc$Calibration$Analysis$Values$Uncertainty <-
        setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
               "uncertRg",
               "1",
               uF1,
               msg)
    return(ccc)
}

