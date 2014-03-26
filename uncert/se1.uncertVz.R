se1.uncertVz <- function(ccc){
    msg   <- "calculated by  se1.uncertV1()"
    a     <- abbrevList(ccc)

    Vz    <- getConstVal(a$cav$Volume, "add")
    Vs    <- getConstVal(a$cav$Volume, "start")
    f     <- getConstVal(a$cav$Expansion, "corr")

    ## Geichung 23 in QSE-SE1-98
    F3   <- 1/(f*Vz/Vs + 1)
    uf3  <- getConstVal(a$cms, "F3_u1")
    
    ccc$Calibration$Analysis$Values$Uncertainty <-
        setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
               "uncertVz",
               "1",
               F3 * uf3,
               msg)
    return(ccc)
}

