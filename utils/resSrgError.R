resSrgError <- function(ccc){
    msg <- "calculated by resSrgError"
    ## - data reduction as given by ToDo
    ## - round

    a        <- abbrevList(ccc)
    p.target <- as.numeric(a$cpt$Values$Pressure$Value)
    maxdev   <- as.numeric(a$cpt$MaxDev)
    k        <- 2 # Erweiterungsfaktor
    ## 10% max. Abw. vom Zieldruck als default
    if(length(maxdev) == 0) maxdev <- 0.10
    
    ##-------------##
    ## pcal
    PCAL <- getSubList(a$cav$Pressure, "cal")
    pcal <- getConstVal(NA, NA, PCAL)

    ##-------------##
    ## uncert_total
    UT <- getSubList(a$cav$Uncertainty, "uncertTotal_rel")
    ut <- getConstVal(NA, NA, UT) * k

    ##-------------##
    ## ind_corr
    PINDcorr <- getSubList(a$cav$Pressure, "ind_corr")
    pindcorr <- getConstVal(NA, NA, PINDcorr)

    ##-------------##
    ## error
    RES <- getSubList(a$cav$Error, "relative")
    result <- getConstVal(NA, NA, RES)
   
    revV        <- median(result)
    noOfP       <- length(p.target)

    ## Ergebnisstabelle soll gleiche Länge wie
    ## target vekcor haben
    td.pcal     <- rep(NA, noOfP)
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
        td.pindcorr[i]  <- unlist(mean(pindcorr[i.take]))
    } #for
    
    PCAL$Value      <- formatC(td.pcal, digits=4, format="E")
    UT$Value        <- formatC(td.ut, digits=1, format="E")
    
    PINDcorr$Value  <- formatC(td.pindcorr, digits=4, format="E")
    UT$Value        <- formatC(td.ut, digits=1, format="E")
    RES$Value       <- formatC(td.result, digits=2, format="E")


    
    ##---------------------------------------------##
    ## table heads
    ##-------------##
    PCAL$HeadCell     <- "{\\(p_{cal}\\)}"
    PCAL$UnitCell     <- PCAL$Unit
    
    UT$HeadCell       <- paste("{\\(U(k=",k,")\\)}", sep="")
    if(UT$Unit == "1"){
        UT$UnitCell    <- ""
    }

    PINDcorr$HeadCell <- "{\\(p_{ind} - p_r\\)}"
    PINDcorr$UnitCell <- PINDcorr$Unit

    if(RES$Unit == "1"){
        RES$Unit       <- ""
    }
    RES$HeadCell      <- "{\\(e\\)}"
    RES$UnitCell      <- RES$Unit
    

    ccc$Calibration$Result$Table      <- list()
    ccc$Calibration$Result$Table[[1]] <- PCAL
    ccc$Calibration$Result$Table[[2]] <- PINDcorr
    ccc$Calibration$Result$Table[[3]] <- RES
    ccc$Calibration$Result$Table[[4]] <- UT
    
    ccc$Calibration$Result$Table[[3]]$Comment <- msg
    
    return(ccc)
}
