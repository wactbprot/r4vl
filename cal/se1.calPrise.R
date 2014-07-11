se1.calPrise <- function(ccc){
    msg <- "calculated by se1.calPcal()"
    a   <- abbrevList( ccc )

    OUTG  <- getSubList(a$cma, "outgas")
    
    if(is.list(OUTG) & OUTG$Unit == "mbar/s"){
        outg    <-  getConstVal(NA,NA,OUTG)
        nOutg   <- length(outg)
        
        tStartMs <- getConstVal(a$cmv, "amt_before") # in ms
        tEndMs   <- getConstVal(a$cmv, "amt_after") # in ms
        dtS      <- (tEndMs - tStartMs)/1000 # in s 
        prise    <- outg[nOutg] * dtS # nur letzter outg-Wert wird benutzt


        ccc$Calibration$Analysis$Values$Pressure <-
            setCcl(ccc$Calibration$Analysis$Values$Pressure,
                   "rise",
                   "mbar",
                   prise,
                   msg)
        
    }
    return(ccc)
}
