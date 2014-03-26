se1.uncertComb <- function(ccc){
    msg   <- "calculated by  se1.uncertComb()"
    a     <- abbrevList(ccc)
    
    uc <- sqrt(
        getConstVal(a$cav$Uncertainty, "uncertPfill")   ^2 +  
        getConstVal(a$cav$Uncertainty, "uncertf")       ^2 + 
        getConstVal(a$cav$Uncertainty, "uncertdT")      ^2 + 
        getConstVal(a$cav$Uncertainty, "uncertT1")      ^2 + 
        getConstVal(a$cav$Uncertainty, "uncertRg")      ^2 + 
        getConstVal(a$cav$Uncertainty, "uncertAds")     ^2 + 
        getConstVal(a$cav$Uncertainty, "uncertVz")      ^2 + 
        getConstVal(a$cav$Uncertainty, "uncertGas")     ^2 + 
        getConstVal(a$cav$Uncertainty, "uncertAtm")     ^2 + 
        getConstVal(a$cav$Uncertainty, "uncertValve")   ^2 + 
        getConstVal(a$cav$Uncertainty, "uncertPres")    ^2 + 
        getConstVal(a$cav$Uncertainty, "uncertRep")     ^2 )
    
    ccc$Calibration$Analysis$Values$Uncertainty <-
        setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
               "uncertComb",
               "1",		
               uc,
               paste(msg, "u(k=1)"))
    return(ccc)
}
