se1.uncertRep <- function(ccc){
    msg   <- "calculated by  se1.uncertRep()"
    a     <- abbrevList(ccc)
    
    f     <- getConstVal(a$cav$Expansion, "corr")
    urep   <- getConstVal(a$cms, "rep_u1")
    
    ccc$Calibration$Analysis$Values$Uncertainty <-
        setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
               "uncertRep",
               "1",		
               rep(urep, length(f)),
               msg)
    return(ccc)
}
