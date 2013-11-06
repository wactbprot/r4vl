getConductIndex <- function(ccc){
    
    a        <- abbrevList(ccc)
    res      <- list()
    lwUnit   <- "l/s"
    pfill    <- getConstVal(a$cav, "fill")
    CFM3     <- getSubList(a$cav, "cnom")
    
    if(length(CFM3$Unit) == 1 & CFM3$Unit == lwUnit){
        ## --- Lw2 --- kl. Lw ---
        lw2List  <- getSubList(a$cms, "useLw2")
        if(lw2List$RangeUnit ==  lwUnit &
           CFM3$Unit         ==  lw2List$RangeUnit){
            
            iLw2 <- which((getConstVal(NA,NA,CFM3) > as.double(lw2List$From)) &
                          (getConstVal(NA,NA,CFM3) < as.double(lw2List$To)))
            
            }else{
                print("getConductIndex: Units don't match")
                stop()
            }
        if(length(iLw2) > 0){
            res$iLw2 <- iLw2  
        }else{
            res$iLw2 <- 0  
        }
        
        ## --- Lw1 --- gr. Lw ---
        lw1List <- getSubList(a$cms, "useLw1")
        if(lw1List$RangeUnit ==  lwUnit &
           CFM3$Unit       == lw1List$RangeUnit){
            iLw1 <- which((getConstVal(NA,NA,CFM3) > as.double(lw1List$From)) &
                          (getConstVal(NA,NA,CFM3) < as.double(lw1List$To)))
        }else{
            print("getConductIndex: Units  don't match")
            stop()
        }
        
        if(length(iLw1) > 0){
            res$iLw1 <- iLw1  
        }else{
            res$iLw1 <- 0  
        }
        
    } # unit == lwUnit

    
    ## --- LwC --- constLw ---
    if(length(iLw2) > 0){
        
        lwCList    <- getSubList(a$cms, "useLwC")
        
        iLwC       <- which(pfill > as.double(lwCList$From) &
                            pfill < as.double(lwCList$To) )
        
        if(length(iLwC) > 0){
            res$iLwC <- iLwC  
        }else{
            res$iLwC <- 0  
        }
    }
    ## -------------------
            
    return(res)
}
