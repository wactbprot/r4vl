se1.calPfill <- function(ccc){
    msg <- "calculated by calPfill()"

    a   <- abbrevList( ccc )
    g   <- a$cmag
    if(a$dataAvailable){

        
        ## -------------------- 10T CDG --------------------------v
        cdg.pfill.10       <- getConstVal(a$cmv, "cdg10_p_fill")
        cdg.offset.10      <- getConatVal(a$cmv, "cdg10_p_fill_offset")
        
        cdg.conv.10        <- getConstVal(a$cmco,"cdg10Conv")
        
        cfcdg10    <- list()
        cfcdg10$a  <- getConstVal(  a$cmco, paste("cdg10CorrA_",g,sep=""))
        cfcdg10$b  <- getConstVal(  a$cmco, paste("cdg10CorrB_",g,sep=""))
        cfcdg10$c  <- getConstVal(  a$cmco, paste("cdg10CorrC_",g,sep=""))
        cfcdg10$d  <- 0
        cfcdg10$e  <- 0
        cfcdg10$f  <- 0

        p.fill.10.uncorr.mbar <- (cdg.pfill.10 - cdg.offset.10) * cdg.conv.10
        pfill.10              <- p.fill.10.uncorr.mbar/(fn.7904(cfcdg10,p.fill.10.uncorr.mbar)/100 + 1)

        
        ccc$Calibration$Analysis$Values$Pressure <-
            setCcl(ccc$Calibration$Analysis$Values$Pressure,
                   "fill",
                   pUnit,
                   ruska.pfill,
                   paste(msg, ""))
        
        return(ccc)
    }
}
