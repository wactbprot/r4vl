resSens <- function(ccc){
    msg <- "calculated by resSens"
    a   <- abbrevList(ccc)


    ##-------------##
    ## pcal
    ##-------------##
    PCAL <- getSubList(a$cav$Pressure, "cal")
    PCAL$HeadCell <- "{\\(p_{cal}\\)}"
    PCAL$UnitCell <- PCAL$Unit
    pcal <- getConstVal(NA, NA, PCAL)

    ##-------------##
    ## uncert_total
    ##-------------##
    k  <- 2
    UT <- getSubList(a$cav$Uncertainty, "uncertTotal_rel")
    UT$HeadCell <- paste("{\\(U(k=",k,")\\)}", sep="")
    if(UT$Unit == "1"){
        UT$UnitCell <- ""
    }
    ut <- getConstVal(NA, NA, UT) * k

    ##-------------##
    ## ind
    ##-------------##
    PIND <- getSubList(a$cav$Pressure, "ind")
    PIND$HeadCell <- "{\\(I_{C}\\)}"
    PIND$UnitCell <-  PIND$Unit
    pind <- getConstVal(NA, NA, PIND)
    ##-------------##
    ## ind_offset
    ##-------------##
    PINDoffs <- getSubList(a$cav$Pressure, "ind_offset")
    PINDoffs$HeadCell <- "{\\(I_{R}\\)}"
    PINDoffs$UnitCell <-  PINDoffs$Unit
    pindoffs <- getConstVal(NA, NA, PINDoffs)

    ##-------------##
    ## ind_corr
    ##-------------##
    PINDcorr <- getSubList(a$cav$Pressure, "ind_corr")
    PINDcorr$HeadCell <- "{\\(I_{C} - I_{R}\\)}"
    PINDcorr$UnitCell <- PINDcorr$Unit
    pindcorr <- getConstVal(NA, NA, PINDcorr)

    ##-------------##
    ## Sensitivity
    ##-------------##
    RES <- getSubList(a$cav$Sensitivity, "gauge_sens")

    RES$HeadCell <- "{\\(S\\)}"
    RES$UnitCell <- RES$Unit
    result <- getConstVal(NA, NA, RES)

    ## Auswirkung des revV
    ## bisher nur auf srg_error
    ## getested
    revV        <- median(result)
    noOfP       <- length(p.target)

    ## Ergebnisstabelle soll gleiche Länge wie
    ## target vekcor haben
    td.pcal     <- rep(NA, noOfP)
    td.pind     <- rep(NA, noOfP)
    td.pindoffs <- rep(NA, noOfP)
    td.pindcorr <- rep(NA, noOfP)
    td.ut       <- rep(NA, noOfP)
    td.result   <- rep(NA, noOfP)

    ## Zieldrücke einzeln durchgehen
    for(i in 1:noOfP){
        i.out  <- NULL
        i.take <- which(pcal >  (p.target[i] *(1- maxdev)) &
                        pcal <  (p.target[i] *(1+ maxdev)))
        msg <- paste(msg,"; For target pressure:",p.target[i],
                     "I take the points:", toString(i.take))

        if(length(i.take) > 1){
            ## ut ist schon k=2, eswerden alle Punkte genommen,
            ## bei dem e.delta kleiner als 3-sigma ist
            ## wobei e.delte die Abweichung vom Reverenzwert ist
            e.delta  <- abs(result[i.take] - revV)
            i.out    <- which(e.delta > mean(ut[i.take])/k*3)

            if(length(i.out) > 0){
                i.take  <- i.take[-i.out]
                msg     <- paste(msg, "from these I skip the points: ",
                                 toString(i.take[i.out]))
            }
        }
        td.pcal[i]      <- unlist(mean(pcal[i.take]))
        td.ut[i]        <- unlist(mean(ut[i.take]))
        td.result[i]    <- unlist(mean(result[i.take]))
        td.pind[i]      <- unlist(mean(pind[i.take]))
        td.pindoffs[i]  <- unlist(mean(pindoffs[i.take]))
        td.pindcorr[i]  <- unlist(mean(pindcorr[i.take]))
    } #for
    
    PCAL$Value      <- formatC(td.pcal, digits=3, format="E")
    UT$Value        <- formatC(td.ut, digits=1, format="E")
    
    PIND$Value      <- formatC(td.pind,     digits=2, format="E")
    PINDoffs$Value  <- formatC(td.pindoffs, digits=1, format="E")
    PINDcorr$Value  <- formatC(td.pindcorr, digits=2, format="E")
    RES$Value       <- formatC(td.result, digits=1, width=3, format="f")

    ccc$Calibration$Result$Table[[1]] <- PCAL
    ccc$Calibration$Result$Table[[2]] <- PIND
    ccc$Calibration$Result$Table[[3]] <- PINDoffs
    ccc$Calibration$Result$Table[[4]] <- PINDcorr
    ccc$Calibration$Result$Table[[5]] <- RES
    ccc$Calibration$Result$Table[[6]] <- UT

    ccc$Calibration$Result$Table[[5]]$Comment <- msg
    return(ccc)
}
