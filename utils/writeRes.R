writeRes <- function( ccc ){
    msg <- "set by writeRes"
    ## - data reduction as given by ToDo
    ## - round

    a <- abbrevList(ccc)

    if(length(a$cp) > 0 &
       length(a$cpt) > 0){

        reType  <- a$cpt$Type
        reTable <- FALSE

        ## pcal
        PCAL <- getSubList(a$cav$Pressure, "cal")
        PCAL$HeadCell <- "{\\(p_{cal}\\)}"
        PCAL$UnitCell <- PCAL$Unit
        pcal <- getConstVal(NA, NA, PCAL)


        ## uncert_total
        k  <- 2
        UT <- getSubList(a$cav$Uncertainty, "uncertTotal_rel")
        UT$HeadCell <- paste("{\\(U(k=",k,")\\)}", sep="")
        if(UT$Unit == "1"){
            UT$UnitCell <- ""
        }
        ut <- getConstVal(NA, NA, UT) * k

        if(reType =="error" | reType =="srg_error"){

            ## ind
            PIND <- getSubList(a$cav$Pressure, "ind")
            PIND$HeadCell <- "{\\(p_{ind}\\)}"
            PIND$UnitCell <-  PIND$Unit
            pind <- getConstVal(NA, NA, PIND)

            ## ind_offset
            PINDoffs <- getSubList(a$cav$Pressure, "ind_offset")
            PINDoffs$HeadCell <- "{\\(p_r\\)}"
            PINDoffs$UnitCell <-  PINDoffs$Unit
            pindoffs <- getConstVal(NA, NA, PINDoffs)

            ## ind_corr
            PINDcorr <- getSubList(a$cav$Pressure, "ind_corr")
            PINDcorr$HeadCell <- "{\\(p_{ind} - p_r\\)}"
            PINDcorr$UnitCell <- PINDcorr$Unit
            pindcorr <- getConstVal(NA, NA, PINDcorr)

            ## rel
            RES <- getSubList(a$cav$Error, "relative")
            if(RES$Unit == "1"){
                RES$Unit <- ""
            }
            RES$HeadCell <- "{\\(e\\)}"
            RES$UnitCell <- RES$Unit
            result <- getConstVal(NA, NA, RES)

            reTable <- TRUE
        }

        if(reType =="sens"){

            ## ind
            PIND <- getSubList(a$cav$Pressure, "ind")
            PIND$HeadCell <- "{\\(I_{C}\\)}"
            PIND$UnitCell <-  PIND$Unit
            pind <- getConstVal(NA, NA, PIND)

            ## ind_offset
            PINDoffs <- getSubList(a$cav$Pressure, "ind_offset")
            PINDoffs$HeadCell <- "{\\(I_{R}\\)}"
            PINDoffs$UnitCell <-  PINDoffs$Unit
            pindoffs <- getConstVal(NA, NA, PINDoffs)
            
            ## ind_corr
            PINDcorr <- getSubList(a$cav$Pressure, "ind_corr")
            PINDcorr$HeadCell <- "{\\(I_{C} - I_{R}\\)}"
            PINDcorr$UnitCell <- PINDcorr$Unit
            pindcorr <- getConstVal(NA, NA, PINDcorr)

            ## rel
            RES <- getSubList(a$cav$Sensitivity, "gauge_sens")
            
            RES$HeadCell <- "{\\(S\\)}"
            RES$UnitCell <- RES$Unit
            result <- getConstVal(NA, NA, RES)

            reTable <- TRUE
        }


        if(reTable){

            ## ---
            ## pressure
            ## ---

            if(length(a$cpt$Values$Pressure) > 0 &
               length(a$cpt$Values$Pressure$Value) > 0){
                p.target <- as.numeric(a$cpt$Values$Pressure$Value)
                maxdev   <- as.numeric(a$cpt$MaxDev)

                if(length(maxdev) == 0) maxdev <- 0.10 ## 10% max. Abw.

                ## Ergebnisstabelle soll gleiche Länge wie
                ## target vekcor haben
                td.pcal     <- p.target
                td.pind     <- p.target
                td.pindoffs <- p.target
                td.pindcorr <- p.target
                td.ut       <- p.target
                td.result     <- p.target

                for(i in 1:length(p.target)){
                    i.take <- which(pcal >  p.target[i] *(1- maxdev) &
                                    pcal <  p.target[i] *(1+ maxdev))


                    if(length(i.take) > 2){
                        u.border <- 0.01
                        e.delta  <- abs(mean(result[i.take]) - result[i.take])

                        msg <- paste(msg,
                                     "; For target pressure:",
                                     p.target[i],
                                     "I take the points:",
                                     toString(i.take))

                        ## Alles was mehr als die Unsicherheit k=1
                        ## abweicht wird nicht mitgenommen

                        i.out  <- which(e.delta > mean(ut[i.take])/2)

                        if(length(i.out) > 0){

                            i.take  <- i.take[-i.out]
                            msg     <- paste(msg,
                                             "from these I skip the points: ",
                                             toString(i.take[i.out]))
                        }
                    }
                    
                    td.pcal[i]      <- mean(pcal[i.take])
                    td.pind[i]      <- mean(pind[i.take])
                    td.pindoffs[i]  <- mean(pindoffs[i.take])
                    td.pindcorr[i]  <- mean(pindcorr[i.take])
                    td.ut[i]        <- mean(ut[i.take])
                    td.result[i]    <- mean(result[i.take])

                }
                
                PCAL$Value          <- formatC(td.pcal, digits=3, format="E")
                UT$Value            <- formatC(td.ut, digits=1, format="E")
                
                if(reType == "srg_error"){
                    PIND$Value      <- formatC(td.pind,     digits=3, format="E")
                    PINDoffs$Value  <- formatC(td.pindoffs, digits=3, format="E")
                    PINDcorr$Value  <- formatC(td.pindcorr, digits=3, format="E")
                    UT$Value        <- formatC(td.ut, digits=1, format="E")
                    RES$Value       <- formatC(td.result, digits=1, width=2, format="E")
                }
                
                if(reType == "error"){
                    PIND$Value      <- formatC(td.pind,     digits=2, format="E")
                    PINDoffs$Value  <- formatC(td.pindoffs, digits=2, format="E")
                    PINDcorr$Value  <- formatC(td.pindcorr, digits=2, format="E")
                    UT$Value        <- formatC(td.ut, digits=1, format="E")
                    RES$Value       <- formatC(td.result, digits=1, width=2, format="E")
                }
                if(reType == "sens"){
                    PIND$Value      <- formatC(td.pind,     digits=2, format="E")
                    PINDoffs$Value  <- formatC(td.pindoffs, digits=2, format="E")
                    PINDcorr$Value  <- formatC(td.pindcorr, digits=2, format="E")
                    RES$Value       <- formatC(td.result, digits=1, width=3, format="f")
                }
                PCAL$Comment        <- msg
                PCAL$Comment        <- msg
                PIND$Comment        <- msg
                PINDoffs$Comment    <- msg
                PINDcorr$Comment    <- msg
                RES$Comment         <- msg
                UT$Comment          <- msg

                ccc$Calibration$Result$Table[[1]] <- PCAL
                ccc$Calibration$Result$Table[[2]] <- PIND
                ccc$Calibration$Result$Table[[3]] <- PINDoffs
                ccc$Calibration$Result$Table[[4]] <- PINDcorr
                ccc$Calibration$Result$Table[[5]] <- RES
                ccc$Calibration$Result$Table[[6]] <- UT
            }
        }
    }
    return(ccc)
}
