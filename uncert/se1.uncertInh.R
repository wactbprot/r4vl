se1.uncertInh <- function(ccc){
    msg   <- "calculated by  se1.uncertInh()"
    a     <- abbrevList(ccc)

    f     <- getConstVal(a$cav$Expansion, "corr")
    
    F7    <- rep(1,length(f))
    uf7   <- getConstVal(a$cms, "F7_u1")
    
    ccc$Calibration$Analysis$Values$Uncertainty <-
        setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
               "uncertGas",
               "1",
               F7 * uf7,
               msg)
    return(ccc)
}
