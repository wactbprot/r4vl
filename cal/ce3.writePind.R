ce3.writePind <- function(ccc){
    msg <- "written by ce3.writePind()"
    a     <- abbrevList(ccc)

    pUnit <- "mbar"

    if(length(a$cpt)>0){
        if(length(a$cpt$Pressures)>0){
            if(length(a$cpt$Pressures$Unit)>0){
                pUnit <- a$cpt$Pressures$Unit
            }
        }
    }

    CAL <- getSubList(a$cav, "cal")
    cal <- getConstVal(NA,NA,CAL)
    
    ## the tribut to god of schemeless design
    IND <- getSubList(a$cmv, "p_ind")

    if(is.null(IND)){
        IND <- getSubList(a$cmv, "ind")
    }

    OFF <- getSubList(a$cmv, "p_ind_offset")
    if(is.null(OFF)){
        OFF <- getSubList(a$cmv, "ind_offset")
    }

    if(is.null(OFF)){
        OFF <- getSubList(a$cmv, "offset")
    }

    ## Druckeinheiten wie Pa, Torr ...
    if(!is.null(IND) &
       !is.null(OFF) &
       IND$Unit != "DCR" &
       IND$Unit != "A"){
        ind <- getConstVal(NA,NA,IND) * getConvFactor(ccc,pUnit,IND$Unit)
        off <- getConstVal(NA,NA,OFF) * getConvFactor(ccc,pUnit,OFF$Unit)
    }

    ## Strom (für Sensitivity)
    if(!is.null(IND)  &
       !is.null(OFF)  &
       IND$Unit == "A"&
       OFF$Unit == "A"){
        pUnit <- "A"
        ind <- getConstVal(NA,NA,IND)
        off <- getConstVal(NA,NA,OFF)
    }

    ## DCR
    if(!is.null(IND) &
       !is.null(OFF) &
       IND$Unit == "DCR" &
       OFF$Unit == "DCR"){
        
        ind   <- getConstVal(NA,NA,IND)
        off   <- getConstVal(NA,NA,OFF)
        
        d     <- getConstVal(a$cmco1, "d")
        rho   <- getConstVal(a$cmco1,"rho" )
        sigma <- getConstVal(a$cmco1,"sigma" )

        dcr   <- ind - off

        R     <- getConstVal(a$cc, "R" )
        T     <- getConstVal(a$cav, "Tuhv")

        if(  a$cmag == "Ar"){
            M <- getConstVal(a$cc, "molWeight_Ar" )
            msg <- paste(msg, "; gas:", a$cmag)
        }
        if(  a$cmag == "N2"){
            M <- getConstVal(a$cc, "molWeight_N2" )
            msg <- paste(msg, "; gas:", a$cmag)
        }
        if(  a$cmag == "D2"){
            M <- getConstVal(a$cc, "molWeight_D2" )
            msg <- paste(msg, "; gas:", a$cmag)
        }

        if(CAL$Unit == "mbar"){

            indUnit  <- "mbar"
            K        <- sqrt(8*R*(T)/(pi*M))*pi*d*rho/2000
            corrind  <- K * dcr/sigma
            ind      <- K * ind
            off      <- K * off
        }
    }

    if(length(ind) > 0 & length(off) > 0){
        ccc$Calibration$Analysis$Values$Pressure <-
            setCcl(ccc$Calibration$Analysis$Values$Pressure, "ind",
                   pUnit,
                   ind,
                   paste(msg)
                   )

        ccc$Calibration$Analysis$Values$Pressure <-
            setCcl(ccc$Calibration$Analysis$Values$Pressure, "ind_offset",
                   pUnit,
                   off,
                   paste(msg)
                   )

        ccc$Calibration$Analysis$Values$Pressure <-
            setCcl(ccc$Calibration$Analysis$Values$Pressure, "ind_corr",
                   pUnit,
                   ind - off,
                   paste(msg, ";p_ind - p_ind_offset")
                   )
    }
    return(ccc)
}
