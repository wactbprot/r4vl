fm3.uncertConstC <- function(ccc){
    msg <- "Calculated by fm3.uncertConstC()"
    
    a   <- abbrevList(ccc)
    
    PFILL     <- getSubList(a$cav, "fill")
    pfill     <- getConstVal(NA,NA,PFILL)
    upfill    <- getConstVal(a$cav, "uncertPfill")     
    pfillUnit <- PFILL$Unit
    
    cnom      <- getConstVal(a$cav, "cnom")

    uncertRes <- rep(0, length(pfill))
   
    iLwC      <-  getConductIndex(ccc)$iLwC
    
    cat( toJSON(getConductIndex(ccc)))
    if(length(iLwC) > 0){
        
        uslope           <- getConstVal(a$cms, "fm3ConstCSlope_u1")
        uintec           <- getConstVal(a$cms, "fm3ConstCIntercept_u1")
        dv2MolCSlope     <- getConstVal(a$cms, "dv2MolCSlope")
        dv2MolCIntercept <- getConstVal(a$cms, "dv2MolCIntercept")
        
        uCabs <- sqrt((pfill[iLwC] * uslope)^2        +
                      (dv2MolCSlope * upfill[iLwC])^2 +
                      ( uintec)^2)
        
        uncertRes[iLwC] <- uCabs/cnom[iLwC]
      
    }
    ccc$Calibration$Analysis$Values$Uncertainty <-
        setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
               "uncertConstC",
               "1",
               uncertRes,
               msg)


    return(ccc)
}
