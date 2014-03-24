fm3.uncertDeltat <- function(ccc){

    msg <- "Calculated by fm3.uncertDeltat()"

    a <- abbrevList(ccc)

    PFILL     <- getSubList(a$cav, "fill")
    pfill     <- getConstVal(NA,NA,PFILL)
    pfillUnit <- PFILL$Unit

    PCAL     <- getSubList(a$cav, "cal")
    pcal     <- getConstVal(NA,NA,PCAL)
    pcalUnit <- PCAL$Unit


    uncertRes <- rep(1,length(pfill))

    ## welcher Lw benutzt wird, wird in getConductIndex() entschieden
    ## die Grenzen sind derzeit im Standard doc def.
    res  <-  getConductIndex(ccc)

    iLw1 <-  res$iLw1
    iLwC <-  res$iLwC
    iLw2 <-  res$iLw2

    ## gibt es für jeden pfill einen Leitwert
    ## gesamte Range
    ## Lw1:

    ## --- iLw1 --- gr.Lw ---
    if(length(iLw1) > 0 ){
        u1     <- getConstVal(a$cms,"fm3Deltat_u1")
        u2List <- getSubList(a$cms,"fm3Deltat_u2")
        iu2    <- checkUncertRange(u2List, PFILL, iLw1)
        
        if(length(iu2) > 0){
            u2             <- getConstVal(NA,NA,u2List)
            uncertRes[iu2] <- sqrt(u1^2 + u2^2)
            
            msg <- paste(msg,
                         "points: ",
                         toString(iu2),
                         " use: ",
                         u2List$Type,
                         "(LW1)")
        }

        u3aList <- getSubList(a$cms,"fm3DeltatLw1_u3_a")
        iu3a    <- checkUncertRange(u3aList, PFILL, iLw1)

        u3bList <- getSubList(a$cms,"fm3DeltatLw1_u3_b")
        iu3b    <- checkUncertRange(u3bList, PFILL, iLw1)

        ## da die beiden Unsicherheiten zusammenhängen,
        ## müssen die letzten Bearbeitungspunkte auch die selben sein
        ## test kann auch wegfallen
        if((pfill[iu3a[length(iu3a)]] == pfill[iu3b[length(iu3b)]]) &
           (length(iu3a) > 0)){
            if(!(pfillUnit =="mbar")){

                stop(paste( pfillUnit," (Unit of pfill) dont match with", u3Lbist$Type))
            }

            u3a <- getConstVal(NA,NA,u3aList)
            u3b <- getConstVal(NA,NA,u3bList)

            uncertRes[iu3a] <- sqrt((u3a + log(pfill[iu3a]/1.0)*u3b)^2 +  u1^2)

            msg <- paste(msg,
                         "points: ",
                         toString(iu3a),
                         " use: ",
                         u3aList$Type,
                         " and ",
                         u3bList$Type,
                         " (LW1)" )
        }
    }##LW1
    ## --- iLw2 ---  kl.Lw ---
    if(length(iLw2) > 0 ){
        u1     <- getConstVal(a$cms,"fm3Deltat_u1")
        u3List <- getSubList(a$cms,"fm3DeltatLw2_u3")
        iu3    <- checkUncertRange(u3List, PFILL, iLw2)

        if(length(iu3) > 0){
            u3             <- getConstVal(NA,NA,u3List)
            uncertRes[iu3] <- sqrt(u1^2+u3^2)
            msg <- paste(msg,
                         "points: ",
                         toString(iu3),
                         " use: ",
                         u3List$Type,
                         " (LW2)")
        }


    }##LW2

    if(length(iLwC) > 0){

        uncertRes[iLwC] <- 0


    }##LWC

    ccc$Calibration$Analysis$Values$Uncertainty <-
        setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
               "uncertDeltat",
               "1",
               uncertRes,
               msg)

    return(ccc)
}
