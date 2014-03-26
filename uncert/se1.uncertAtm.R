se1.uncertAtm <- function(ccc){
    msg   <- "calculated by  se1.uncertAtm()"
    a     <- abbrevList(ccc)

    f     <- getConstVal(a$cav$Expansion, "corr")
    
    F5    <- rep(1,length(f))
    uf5   <- getConstVal(a$cms, "F5_u1")
    
    ccc$Calibration$Analysis$Values$Uncertainty <-
        setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
               "uncertAtm",
               "1",
               F5 * uf5,
               msg)
    return(ccc)
}
