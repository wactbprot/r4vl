se1.uncertAds <- function(ccc){
    msg   <- "calculated by  se1.uncertAds()"
    a     <- abbrevList(ccc)

    pcal  <- getConstVal(a$cav, "cal") # in mbar
    
    ## -- Adsorption
    UF2a <- getSubList(a$cms, "F2_u1_a") ## 

    UF2b <- getSubList(a$cms, "F2_u1_b") ##
    UF2  <- getSubList(a$cms, "F2_u2") ## 

    uF2  <- rep(NA, length(pcal))

    i.u1 <- which(pcal >  as.numeric(UF2a$From) &
                  pcal <  as.numeric(UF2a$To))
    i.u2 <- which(pcal >  as.numeric(UF2$From) &
                  pcal <  as.numeric(UF2$To))

    if(length(i.u1) > 0){
        uF2[i.u1] <- as.numeric(UF2a$Value) + as.numeric(UF2a$Value) * pcal[i.u1] 
    }

    if(length(i.u2) > 0){
        uF2[i.u2] <- as.numeric(UF2$Value)
    }

    ccc$Calibration$Analysis$Values$Uncertainty <-
        setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
               "uncertAds",
               "1",
               uF2,
               msg)

    
    return(ccc)
}
