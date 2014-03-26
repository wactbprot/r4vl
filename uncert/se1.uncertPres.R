se1.uncertPres <- function(ccc){
    msg   <- "calculated by  se1.uncertPres()"
    a     <- abbrevList(ccc)

    pcal  <- getConstVal(a$cav$Pressure, "cal")
    
   
    upres   <- getConstVal(a$cms, "pres_u1")
    
    ccc$Calibration$Analysis$Values$Uncertainty <-
        setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
               "uncertPres",
               "1",
               upres/pcal,
               msg)
    return(ccc)
}
