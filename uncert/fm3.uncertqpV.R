fm3.uncertqpV <- function(ccc){

    msg <- "Calculated by fm3.uncertqpV()"
    a   <- abbrevList(ccc)

    pfill        <-  getConstVal(a$cav, "fill")
    uncertRes    <-  rep(0,length(pfill))

    uDPfillList  <- getSubList(a$cav, "uncertDPfill")
    uPfillList   <- getSubList(a$cav, "uncertPfill")
    uDVList      <- getSubList(a$cav, "uncertDeltaV")
    uDtList      <- getSubList(a$cav, "uncertDeltat")
    uDVDtList    <- getSubList(a$cav, "uncertDeltaVDeltat")
    uConstLwList <- getSubList(a$cav, "uncertConstLw")
    
    if((uPfillList$Unit  ==  uDPfillList$Unit) &
       (uPfillList$Unit  ==  uDVList$Unit)     &
       (uPfillList$Unit  ==  uDVDtList$Unit)   &
       (uPfillList$Unit  ==  uDtList$Unit)     &
       (uPfillList$Unit  ==  uConstLwList$Unit)&
       (uPfillList$Unit  ==  "1")){

        uDPfill  <- getConstVal( NA,NA,  uDPfillList )
        uPfill   <- getConstVal( NA,NA,  uPfillList  )
        uDV      <- getConstVal( NA,NA,  uDVList     )
        uDt      <- getConstVal( NA,NA,  uDtList     )
        uDVDt    <- getConstVal( NA,NA,  uDVDtList   )
        uConstLw <- getConstVal( NA,NA,  uConstLwList)

        
        uncertRes[igfu] <- sqrt(uDPfill^2 +
                                uPfill^2 +
                                uDV^2 +
                                uDt^2 +
                                uDVDt^2 +
                                uConstLw^2)

    }

    ccc$Calibration$Analysis$Values$Uncertainty <-
        setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
               "uncertqpV",
               "1",
               uncertRes,
               msg)

    return(ccc)

}
