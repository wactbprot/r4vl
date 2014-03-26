se1.uncertValve <- function(ccc){
    msg   <- "calculated by  se1.uncertValve()"
    a     <- abbrevList(ccc)

    f     <- getConstVal(a$cav$Expansion, "corr")
    
    F6    <- rep(1,length(f))
    uf6   <- getConstVal(a$cms, "F6_u1")
    
    ccc$Calibration$Analysis$Values$Uncertainty <-
        setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
               "uncertValve",
               "1",
               F6 * uf6,
               msg)
    return(ccc)
}
