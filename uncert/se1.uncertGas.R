se1.uncertGas <- function(ccc){
    msg   <- "calculated by  se1.uncertV1()"
    a     <- abbrevList(ccc)

    f     <- getConstVal(a$cav$Expansion, "corr")
    
    F4    <- rep(1,length(f))
    uf4   <- getConstVal(a$cms, "F4_u1")
    
    ccc$Calibration$Analysis$Values$Uncertainty <-
        setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
               "uncertGas",
               "1",
               F4 * uf4,
               msg)
    return(ccc)
}
