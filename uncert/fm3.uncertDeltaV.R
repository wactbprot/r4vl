fm3.uncertDeltaV <- function(ccc){
    msg <- "Calculated by fm3.uncertDeltaV()"
    a <- abbrevList(ccc)

    PFILL <- getSubList(a$ca, "fill")
    pfill     <- getConstVal(NA,NA,PFILL)
    noOfPfill <- length(pfill)

    uncertDeltaV <- rep(0,noOfPfill)

    if(a$cmscok == "opK1" |a$cmscok == "opK2" |a$cmscok == "opK3" ){

        pfillUnit <- PFILL$Unit

        u2aList <-  getSubList(a$cms,"fm3DeltaV_u2_a")
        u2aAbs <- getConstVal(NA,NA,u2aList)

        u2cList <-  getSubList(a$cms,"fm3DeltaV_u2_c")
        u2cAbs <- getConstVal(NA,NA,u2cList)

        deltaGList <-  getSubList(a$cms,"deltaG")
        deltaG <- getConstVal(NA,NA,deltaGList)

        if(u2aList$Unit ==  deltaGList$Unit){
            u2a <-  u2aAbs/deltaG
        }else{
            stop("unit deltaG and fm3DeltaV_u2_a don't match")
        }
        if(u2cList$Unit ==  deltaGList$Unit){
            u2c <-  u2cAbs/deltaG
        }else{
            stop("unit deltaG and fm3DeltaV_u2_c don't match")
        }

        u2bList <- getSubList(a$cms,"fm3DeltaV_u2_b")
        u2b <-  getConstVal(NA,NA,u2bList)
        iu2b <- checkUncertRange(u2bList, PFILL)

        u2dList <- getSubList(a$cms,"fm3DeltaV_u2_d")
        u2d <-  getConstVal(NA,NA,u2dList)
        iu2d <- checkUncertRange(u2dList, PFILL)

        u2eList <- getSubList(a$cms,"fm3DeltaV_u2_e")
        u2e <-  getConstVal(NA,NA,u2eList)
        iu2e <- checkUncertRange(u2eList, PFILL)

        u2fList <- getSubList(a$cms,"fm3DeltaV_u2_f")
        u2f <-  getConstVal(NA,NA,u2fList)
        iu2f <- checkUncertRange(u2fList, PFILL)

        u2gList <- getSubList(a$cms,"fm3DeltaV_u2_g")
        u2g <-  getConstVal(NA,NA,u2gList)
        iu2g <- checkUncertRange(u2gList, PFILL)

        ## an together:
        uDeltaG            <- sqrt(u2a^2 + u2b^2 + u2c^2)
        uA                 <- sqrt(uDeltaG^2 + u2d^2 + u2e^2 + u2g^2)
        uncertDeltaV[iu2g] <- sqrt(uA^2 + u2e^2 + u2f^2)
        
     
    }
    ccc$Calibration$Analysis$Values$Uncertainty <-
        setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
               "uncertDeltaV",
               "1",
               uncertDeltaV,
               msg)
    
    return(ccc)
}
