writeRes <- function( ccc ){
    msg <- "set by writeRes"
    ## - data reduction as given by ToDo
    ## - round

    a <- abbrevList(ccc)
    if(length(a$cp) > 0 &
       length(a$cpt) > 0){
        reType <- a$cpt$Type

        ## cal
        PCAL <- getSubList(a$cav$Pressure, "cal")
        PCAL$HeadCell <- "$p_{cal}$"
        PCAL$UnitCell <- PCAL$Unit
        pcal <- getConstVal(NA, NA, PCAL)
        PCAL$Value <- formatC(pcal, digits=3, format="E")

        ## ind
        PIND <- getSubList(a$cav$Pressure, "ind")
        PIND$HeadCell <- "$p_{ind}$"
        PIND$UnitCell <-  PIND$Unit
        pind <- getConstVal(NA, NA, PIND)
        PIND$Value <- formatC(pind, digits=2, format="E")

        ## ind_offset
        PINDoffs <- getSubList(a$cav$Pressure, "ind_offset")
        PINDoffs$HeadCell <- "$p_r$"
        PINDoffs$UnitCell <-  PINDoffs$Unit
        pindoffs <- getConstVal(NA, NA, PINDoffs)
        PINDoffs$Value <- formatC(pindoffs, digits=2, format="E")

        ## ind_corr
        PINDcorr <- getSubList(a$cav$Pressure, "ind_corr")
        PINDcorr$HeadCell <- "$p_{ind} - p_r$"
        PINDcorr$UnitCell <- PINDcorr$Unit
        pindcorr <- getConstVal(NA, NA, PINDcorr)
        PINDcorr$Value <- formatC(pindcorr, digits=2, format="E")

        if(reType =="error"){
            ## rel
            EREL <- getSubList(a$cav$Error, "relative")
            if(EREL$Unit == "1"){
                EREL$Unit <- ""
            }
            EREL$HeadCell <- "$e$"
            EREL$UnitCell <- EREL$Unit
            erel <- getConstVal(NA, NA, EREL)
            EREL$Value <- formatC(erel, digits=1, width=2, format="E")

            ## ---
            ## sort out function
            ## ---

            ccc$Calibration$Result$Table[[1]] <- PCAL
            ccc$Calibration$Result$Table[[2]] <- PIND
            ccc$Calibration$Result$Table[[3]] <- PINDoffs
            ccc$Calibration$Result$Table[[4]] <- PINDcorr
            ccc$Calibration$Result$Table[[5]] <- EREL

        }

    }

    return(ccc)

}
