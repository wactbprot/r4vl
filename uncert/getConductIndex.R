getConductIndex <- function(ccc){
    
    a        <- abbrevList(ccc)
    lwUnit   <- "l/s"
    pfill    <- getConstVal(a$cav, "fill")
    cnom     <- getConstVal(a$cav, "cnom")
 
    ## --- Lw2 --- kl. Lw ---
    lw2List  <- getSubList(a$cms, "useLw2")

    iLw2     <- which((cnom > as.double(lw2List$From)) &
                      (cnom < as.double(lw2List$To)))
    
    ## --- Lw1 --- gr. Lw ---
    lw1List  <- getSubList(a$cms, "useLw1")

    iLw1     <- which((cnom > as.double(lw1List$From)) &
                      (cnom < as.double(lw1List$To)))

    
    ## --- LwC --- constLw ---
    lwCList    <- getSubList(a$cms, "useLwC")
    
    iLwC       <- which(pfill > as.double(lwCList$From) &
                        pfill < as.double(lwCList$To) )
    
    if(length(iLwC) > 0){
        iout2 <- which(iLwC %in% iLw2)
        if(length( iout2) == length(iLw2)){
            iLw2 <- integer(0)
        }else{
            iLw2 <- iLw2[-iout2]
        }
    }
    ## -------------------
    
    return(list(iLw1 = iLw1,
                iLw2 = iLw2,
                iLwC = iLwC))
}
