se1.calPfill <- function(ccc){
    
    msg <- "calculated by se1.calPfill()"
    
    a      <- abbrevList( ccc )
    g      <- a$cmag
    pUnit  <- "mbar"
    N      <- length(getConstVal(a$cmv$Expansion, "ratio_uncorr"))

    pfill                <- rep(NA, N)
    offset               <- rep(NA, N)
    cdg.offset.10.mbar   <- rep(NA, N)
    cdg.offset.100.mbar  <- rep(NA, N)
    cdg.offset.1000.mbar <- rep(NA, N)
    ## der offset kann mehrmals gemessen werden
    ## welcher für welchen benutzt wird
    ## wird über die Zeit entschieden
    ofMt      <- getConstVal(a$cma$Time, "amt_offset")
    beMt      <- getConstVal(a$cmv$Time, "amt_before")


    if(a$dataAvailable){
        ## -------------------- 10T CDG --------------------------v

        cdg.conv.10             <- getConstVal(a$cmco,"cdg10Conv")
        cdg.pfill.10.mbar       <- getConstVal(a$cmv, "cdg10_p_fill")          * cdg.conv.10
        
        ## ----offset---
        cdg.offset.10           <- getConstVal(a$cma, "cdg10_p_fill_offset")  
        for(k in 1:length(ofMt)){
            ## 
            u                     <- which( beMt > ofMt[k])
            cdg.offset.10.mbar[u] <- cdg.offset.10[k] * cdg.conv.10
        }
        ## ------------
        
        cfcdg10        <- list()
        cfcdg10$a      <- getConstVal(  a$cmco, paste("cdg10CorrA_",g,sep=""))
        cfcdg10$b      <- getConstVal(  a$cmco, paste("cdg10CorrB_",g,sep=""))
        cfcdg10$c      <- getConstVal(  a$cmco, paste("cdg10CorrC_",g,sep=""))
        cfcdg10$d      <- getConstVal(  a$cmco, paste("cdg10CorrD_",g,sep=""))
        cfcdg10$e      <- getConstVal(  a$cmco, paste("cdg10CorrE_",g,sep=""))
        cfcdg10$f      <- 0

        p.fill.10.uncorr.mbar   <- cdg.pfill.10.mbar - cdg.offset.10.mbar
        E.10                    <- fn.4403(cfcdg10, p.fill.10.uncorr.mbar)
        pfill.10.mbar           <- p.fill.10.uncorr.mbar/(E.10/100 + 1)

        RANGE.10       <- getSubList(a$cmco, "cdg10UseDev")
        
        i.10           <- which((pfill.10.mbar > as.numeric(RANGE.10$From)) &
                                (pfill.10.mbar < as.numeric(RANGE.10$To)))

        pfill[i.10]    <- pfill.10.mbar[i.10]
        offset[i.10]   <- cdg.offset.10.mbar[i.10]
                          
        
        ## -------------------- 100T CDG --------------------------v
        cdg.conv.100               <- getConstVal(a$cmco,"cdg100Conv")
        
        cdg.pfill.100.mbar         <- getConstVal(a$cmv, "cdg100_p_fill")        * cdg.conv.100

        ## ----offset---
        cdg.offset.100             <- getConstVal(a$cma, "cdg100_p_fill_offset")  
        for(k in 1:length(ofMt)){
            ## 
            u                      <- which( beMt > ofMt[k])
            cdg.offset.100.mbar[u] <- cdg.offset.100[k] * cdg.conv.100
        }
        ## ------------

        cfcdg100       <- list()
        cfcdg100$a     <- getConstVal(a$cmco, "cdg100CorrA")
        cfcdg100$b     <- getConstVal(a$cmco, "cdg100CorrB")
        cfcdg100$c     <- getConstVal(a$cmco, "cdg100CorrC")
        cfcdg100$d     <- getConstVal(a$cmco, "cdg100CorrD")
        cfcdg100$e     <- getConstVal(a$cmco, "cdg100CorrE")
        cfcdg100$f     <- 0

        p.fill.100.uncorr.mbar  <- cdg.pfill.100.mbar - cdg.offset.100.mbar
        E.100                   <- fn.4403(cfcdg100, p.fill.100.uncorr.mbar)
        pfill.100.mbar          <- p.fill.100.uncorr.mbar/(E.100/100 + 1)

        RANGE.100      <- getSubList(a$cmco, "cdg100UseDev")
        
        i.100          <- which((pfill.100.mbar > as.numeric(RANGE.100$From)) &
                          (pfill.100.mbar < as.numeric(RANGE.100$To)))
        
        pfill[i.100]   <- pfill.100.mbar[i.100]
        offset[i.100]  <- cdg.offset.100.mbar[i.100]
        
        ## -------------------- 1000T CDG --------------------------v
        cdg.conv.1000               <- getConstVal(a$cmco,"cdg1000Conv")
        
        cdg.pfill.1000.mbar         <- getConstVal(a$cmv, "cdg1000_p_fill")        * cdg.conv.1000

        ## ----offset---
        cdg.offset.1000             <- getConstVal(a$cma, "cdg1000_p_fill_offset")  
        for(k in 1:length(ofMt)){
            ## 
            u                      <- which( beMt > ofMt[k])
            cdg.offset.1000.mbar[u] <- cdg.offset.1000[k] * cdg.conv.1000
        }
        ## ------------

        cfcdg1000       <- list()
        cfcdg1000$a     <- getConstVal(a$cmco, "cdg1000CorrA")
        cfcdg1000$b     <- getConstVal(a$cmco, "cdg1000CorrB")
        cfcdg1000$c     <- getConstVal(a$cmco, "cdg1000CorrC")
        cfcdg1000$d     <- getConstVal(a$cmco, "cdg1000CorrD")
        cfcdg1000$e     <- getConstVal(a$cmco, "cdg1000CorrE")
        cfcdg1000$f     <- 0

        p.fill.1000.uncorr.mbar  <- cdg.pfill.1000.mbar - cdg.offset.1000.mbar
        E.1000                   <- fn.4403(cfcdg1000, p.fill.1000.uncorr.mbar)
        pfill.1000.mbar          <- p.fill.1000.uncorr.mbar/(E.1000/100 + 1)

        RANGE.1000      <- getSubList(a$cmco, "cdg1000UseDev")
        
        i.1000          <- which((pfill.1000.mbar > as.numeric(RANGE.1000$From)) &
                          (pfill.1000.mbar < as.numeric(RANGE.1000$To)))
        
        pfill[i.1000]   <- pfill.1000.mbar[i.1000]
        offset[i.1000]  <- cdg.offset.1000.mbar[i.1000]

        ## ---------------------------------------------------------^
       
        ccc$Calibration$Analysis$Values$Pressure <-
            setCcl(ccc$Calibration$Analysis$Values$Pressure,
                   "fill",
                   pUnit,
                   pfill,
                   paste(msg, "pfill = pindfill - poffset"))
       
        ccc$Calibration$Analysis$Values$Pressure <-
            setCcl(ccc$Calibration$Analysis$Values$Pressure,
                   "fill_offset",
                   pUnit,
                   offset,
                   paste(msg, "offset nur zu Kontrollzwecken; ist bei pfill schon subtrahiert"))

        
        ccc$Calibration$Analysis$Values$Pressure <-
            setCcl(ccc$Calibration$Analysis$Values$Pressure,
                   "fill_1000",
                   pUnit,
                   pfill.1000.mbar,
                   paste(msg, "Übereinstimmung der CDGs"))

        
        ccc$Calibration$Analysis$Values$Pressure <-
            setCcl(ccc$Calibration$Analysis$Values$Pressure,
                   "fill_100",
                   pUnit,
                   pfill.100.mbar,
                   paste(msg, "Übereinstimmung der CDGs"))

        
        ccc$Calibration$Analysis$Values$Pressure <-
            setCcl(ccc$Calibration$Analysis$Values$Pressure,
                   "fill_10",
                   pUnit,
                   pfill.10.mbar,
                   paste(msg, "Übereinstimmung der CDGs"))

        ccc$Calibration$Analysis$Values$Error <-
            setCcl(ccc$Calibration$Analysis$Values$Error,
                   "e_fill_10",
                   "%",
                   E.10,
                   paste(msg, "e in % des pfill"))

        ccc$Calibration$Analysis$Values$Error <-
            setCcl(ccc$Calibration$Analysis$Values$Error,
                   "e_fill_100",
                   "%",
                   E.100,
                   paste(msg, "e in % des pfill"))

        ccc$Calibration$Analysis$Values$Error <-
            setCcl(ccc$Calibration$Analysis$Values$Error,
                   "e_fill_1000",
                   "%",
                   E.1000,
                   paste(msg, "e in % des pfill"))


        
    }
    
    return(ccc)
    
}
