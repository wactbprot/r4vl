## dringend aufräumen!

ce3.newCalPfill <- function(ccc){
    msg   <- "calculated by ce3.newCalPfill"

    a      <- abbrevList(ccc)
    pUnit  <- "mbar"
    gas    <- a$cmscg
    
    ## muss noch aus db kommen
    srg.border     <- 1e-2
    
    cdgax001border <- 0.10
    cdgax01border  <- 1.33
    cdgax1border   <- 13.3

    cdgbx001border <- cdgax1border
    cdgbx01border  <- 133
    cdgbx1border   <- 1330

    ## NB:
    ## das ganze Verfahren wie der pfill
    ## nach analyse kommt (offset korr. ...)
    ## muss evtl nochmal überarb. werden
    ## todo!
    apre <- "cdga_"
    bpre <- "cdgb_"
    cpre <- "cdgc_"
    suf  <- "_offset"

    sVpoax001 <-  getConstVal(a$cma$Pressure, paste(apre,"x0.01",suf,sep=""))
    sVpoax01  <-  getConstVal(a$cma$Pressure, paste(apre,"x0.1" ,suf,sep=""))
    sVpoax1   <-  getConstVal(a$cma$Pressure, paste(apre,"x1"   ,suf,sep=""))

    sVpobx001 <-  getConstVal(a$cma$Pressure, paste(bpre,"x0.01",suf,sep=""))
    sVpobx01  <-  getConstVal(a$cma$Pressure, paste(bpre,"x0.1" ,suf,sep=""))
    sVpobx1   <-  getConstVal(a$cma$Pressure, paste(bpre,"x1"   ,suf,sep=""))

    sVpocx001 <-  getConstVal(a$cma$Pressure, paste(cpre,"x0.01",suf,sep=""))
    sVpocx01  <-  getConstVal(a$cma$Pressure, paste(cpre,"x0.1" ,suf,sep=""))
    sVpocx1   <-  getConstVal(a$cma$Pressure, paste(cpre,"x1"   ,suf,sep=""))

    ## der offset kann mehrmals gemessen werden
    ## welcher für welchen benutzt wird
    ## wird über die Zeit entschieden

    if(a$cmscok == "opK1" |a$cmscok == "opK2"|a$cmscok == "opK3"){
        ofMt      <- getConstVal(a$cma$Time, "offset_mt")
        lwMt      <- getConstVal(a$cma$Time, "start_lw")
    } 

    if(a$cmscok == "opK4"){
        ofMt      <- getConstVal(a$cma$Time, "offset_mt")
        lwMt      <- getConstVal(a$cma$Time, "begin_constC")
        
    } 

    navec     <- rep(NA,length(lwMt))

    poax001 <- navec
    poax01  <- navec
    poax1   <- navec
    pobx001 <- navec
    pobx01  <- navec
    pobx1   <- navec
    pocx001 <- navec
    pocx01  <- navec
    pocx1   <- navec
   
    for(k in 1:length(ofMt)){
        ##
        u               <- which( lwMt > ofMt[k])
        ## wo Messzeitpunkt größer Offsetmesszeit
        
        poax001[u]      <-  sVpoax001[k]
        poax01[u]       <-  sVpoax01[k]
        poax1[u]        <-  sVpoax1[k]
        pobx001[u]      <-  sVpobx001[k]
        pobx01[u]       <-  sVpobx01[k]
        pobx1[u]        <-  sVpobx1[k]
        pocx001[u]      <-  sVpocx001[k]
        pocx01[u]       <-  sVpocx01[k]
        pocx1[u]        <-  sVpocx1[k]
    }
   
    ## gemessen direkt nach usr-input ok
    PF    <- getSubList(a$cmv, "fill")
    pfill <- getConstVal(NA, NA, PF)

    pfilloffset <- rep(NA,length(pfill))

    if(a$cmscok == "opK1" |a$cmscok == "opK2"|a$cmscok == "opK3"){
        ## zur Korr. der Druckdiff. beim Schließen des V22
        ## (dpC wird nur in X1 Range gemessen
        dpC <- getConstVal(a$cmv,"drift_mean_p") - pocx1

        ## haben die gleiche Unit wie pfill
        bdf        <- getConstVal(a$cmv, "before_drift_fill")
        adf        <- getConstVal(a$cmv, "after_drift_fill")
        pdriftfill <- (bdf + adf)/2

        icdga      <- which( pdriftfill <= cdgax1border)
        icdgb      <- which( pdriftfill >  cdgax1border)

        if(length(icdga > 0)){
            msg      <- paste(msg,"assume cdga for points:", toString(icdga))

            ix001    <- which(pdriftfill < cdgax001border)
            ix01     <- which((pdriftfill < cdgax01border) & (pdriftfill > cdgax001border))
            ix1      <- which((pdriftfill < cdgax1border ) & (pdriftfill > cdgax01border))

            if(length(ix1) > 0){
                pdriftfill[ix1]       <- pdriftfill[ix1]   -  poax1[ix1]
                pfill[ix1]            <- pfill[ix1]        -  poax1[ix1]
                pfilloffset[ix1]      <- poax1[ix1]
            }

            if(length(ix01)>0){
                pdriftfill[ix01]      <- pdriftfill[ix01] -  poax01[ix01]
                pfill[ix01]           <- pfill[ix01]      -  poax01[ix01]
                pfilloffset[ix01]     <- poax01[ix01]

            }
            if(length(ix001)>0){
                pdriftfill[ix001]     <- pdriftfill[ix001]-  poax001[ix001]
                pfill[ix001]          <- pfill[ix001]     -  poax001[ix001]
                pfilloffset[ix001]    <- poax001[ix001]
            }
            ## Fehlerkorrekturen:
            ## cdga
            ## gasartabh.
            ## F(relativ)=(a+c*pind+e*pind^2)/(1+b*pind+d*pind^2+f*pind^3)
            if(gas == "Ar" || gas == "N2"){
                g <- gas
            }else{
                g <- "N2"
                msg <- paste(msg, "no calibration for CDGA for gas: ", gas, ". Use N2 instead")
            }

            cfcdga    <- list()
            cfcdga$a  <- getConstVal(  a$cmco, paste("cdgaCorrA_",g,sep=""))
            cfcdga$b  <- getConstVal(  a$cmco, paste("cdgaCorrB_",g,sep=""))
            cfcdga$c  <- getConstVal(  a$cmco, paste("cdgaCorrC_",g,sep=""))
            cfcdga$d  <- getConstVal(  a$cmco, paste("cdgaCorrD_",g,sep=""))
            cfcdga$e  <- getConstVal(  a$cmco, paste("cdgaCorrE_",g,sep=""))
            cfcdga$f  <- getConstVal(  a$cmco, paste("cdgaCorrF_",g,sep=""))


            pdriftfill[icdga] <- pdriftfill[icdga]/(fn.7904(cfcdga,pdriftfill[icdga]) + 1)
            pfill[icdga]      <- pfill[icdga]/(fn.7904(cfcdga,pfill[icdga]) + 1)

        }# cdga

        if(length(icdgb > 0)){
            msg <- paste(msg,"assume cdgb for points:", toString(icdgb))

            ix01  <- which((pdriftfill < cdgbx01border) & (pdriftfill > cdgbx001border))
            ix1   <- which((pdriftfill < cdgbx1border)  & (pdriftfill > cdgbx01border))

            if(length(ix1)>0){
                pdriftfill[ix1]     <- pdriftfill[ix1]   -  pobx1[ix1]
                pfill[ix1]          <- pfill[ix1]        -  pobx1[ix1]

                pfilloffset[ix1]    <- pobx1[ix1]

            }

            if(length(ix01)>0){
                pdriftfill[ix01]    <- pdriftfill[ix01]  -  pobx01[ix01]
                pfill[ix01]         <- pfill[ix01]       -  pobx01[ix01]

                pfilloffset[ix01]   <- pobx01[ix01]
            }


            ## Fehlerkorrekturen:
            ## cdgb
            ## gasartunabh.
            ## F(relativ)=(a+c*pind+e*pind^2)/(1+b*pind+d*pind^2+f*pind^3)
            cfcdgb    <- list()
            cfcdgb$a  <- getConstVal(  a$cmco, "cdgbCorrA")
            cfcdgb$b  <- getConstVal(  a$cmco, "cdgbCorrB")
            cfcdgb$c  <- getConstVal(  a$cmco, "cdgbCorrC")
            cfcdgb$d  <- getConstVal(  a$cmco, "cdgbCorrD")
            cfcdgb$e  <- getConstVal(  a$cmco, "cdgbCorrE")
            cfcdgb$f  <- getConstVal(  a$cmco, "cdgbCorrF")

            pdriftfill[icdgb] <- pdriftfill[icdgb]/(fn.7904(cfcdgb,pdriftfill[icdgb]) + 1)
            pfill[icdgb]      <- pfill[icdgb]/(fn.7904(cfcdgb,pfill[icdgb]) + 1)

        }# cdgb

        ## zum Fülldruck der Leitwertmessung
        ## kommt noch die Druckdifferenz
        ## durch das Schließen des V22
        ## bei pfill ist das nicht dabei,
        ## weil hier V22 offen ist
        pdriftfill <- pdriftfill + dpC


        ccc$Calibration$Analysis$Values$Pressure <-
            setCcl(ccc$Calibration$Analysis$Values$Pressure,
                   "lw",
                   PF$Unit,
                   pdriftfill,
                   msg)

        ccc$Calibration$Analysis$Values$Pressure <-
            setCcl(ccc$Calibration$Analysis$Values$Pressure,
                   "fill",
                   PF$Unit,
                   pfill,
                   msg)

        ccc$Calibration$Analysis$Values$Pressure <-
            setCcl(ccc$Calibration$Analysis$Values$Pressure,
                   "fill_offset",
                   PF$Unit,
                   pfilloffset,
                   "wird nur aus Kontrollgründen gespeichert; bei pfill ist der Offset schon subtrahiert")

        ccc$Calibration$Analysis$Values$Pressure <-
            setCcl(ccc$Calibration$Analysis$Values$Pressure,
                   "dpC",
                   PF$Unit,
                   dpC,
                   msg)
    }

    if(a$cmscok == "opK4"){

        
        
        icdga      <- which( pfill <= cdgax1border)
        isrg       <- which(pfill  <= srg.border)
        ## ------------ CDGA 10T 
        if(length(icdga > 0)){
            
            msg      <- paste(msg,"assume cdga for points:", toString(icdga))
            ix001    <- which(pfill < cdgax001border)


            if(length(ix001)>0){
                pfill[ix001]          <- pfill[ix001]     -  poax001[ix001]
                pfilloffset[ix001]    <- poax001[ix001]
                msg <- paste(msg, "assume Range X0.01 for points:", toString(icdga))
            }
            
            if(gas == "Ar" || gas == "N2"){
                g <- gas
            }else{
                g <- "N2"
                msg <- paste(msg,
                             "no calibration for CDGA for gas: ",
                             g,
                             ". Use N2 instead")
            }
            
            cfcdga    <- list()
            cfcdga$a  <- getConstVal(  a$cmco, paste("cdgaCorrA_",g,sep=""))
            cfcdga$b  <- getConstVal(  a$cmco, paste("cdgaCorrB_",g,sep=""))
            cfcdga$c  <- getConstVal(  a$cmco, paste("cdgaCorrC_",g,sep=""))
            cfcdga$d  <- getConstVal(  a$cmco, paste("cdgaCorrD_",g,sep=""))
            cfcdga$e  <- getConstVal(  a$cmco, paste("cdgaCorrE_",g,sep=""))
            cfcdga$f  <- getConstVal(  a$cmco, paste("cdgaCorrF_",g,sep=""))
            
            F             <- fn.7904(cfcdga,pfill[icdga])
            pfill[icdga]  <- pfill[icdga]/(F + 1)
        }

        ## SRG F11
        ##
        ## opK4: sehr kleine Fülldrücke die erst noch
        ## berechnet/korrigiert werden müssen
        ##
        
        noOfViscIter <- 5 ## Anzahl der Iterationen zu visc. Korrektur

        srgInd   <- getSubList(a$cm, "srg_fill")
        srgOff   <- getSubList(a$cm, "srg_offset")

        if(length(srgInd$Value) > 0){

            if((srgInd$Unit == "DCR")){
                
                d            <- getConstVal(a$cmco,"d")
                rho          <- getConstVal(a$cmco,"rho")
                R            <- getConstVal(a$cc,"R")
                
                pfillDCR        <- getConstVal(NA,NA,srgInd) 
                poffsetDCR      <- getConstVal(NA,NA,srgOff) # in DCR
                
                if(a$cmscg =="N2"){
                    sigma      <- getConstVal(a$cmco,"sigma_N2")
                    slope      <- getConstVal(a$cmco,"slope_N2")

                    M          <- getConstVal(a$cc,"molWeight_N2")
                    visc       <- getConstVal(a$cc,"visc_N2")
                }

                if(a$cmscg =="Ar"){
                    M           <- getConstVal(a$cc,"molWeight_Ar")
                    visc        <- getConstVal(a$cc,"visc_Ar")
                    sigma       <- getConstVal(a$cmco,"sigma_Ar")
                    slope       <- getConstVal(a$cmco,"slope_Ar")
                }

                T     <- getConstVal(a$ca,"Tfm3")

                K <- sqrt(8*R*(T)/(pi*M))*pi*d*rho/sigma/2000
                pfillMbar <- K * pfillDCR ## mbar
                poffsetMbar <- K * poffsetDCR ## mbar

                panz      <- pfillMbar - poffsetMbar 
                pfillKorr <- panz

                for(i in 1:noOfViscIter){

                    pfillKorr <- panz * sigma/(slope * pfillKorr + sigma)
                   
                }

                pfill[isrg] <- pfillKorr[isrg]
                msg <- paste(msg,
                             " from dcr values with ",
                             noOfViscIter,
                             " iterations for viscosity correction" )
            }## Unit dcr
        }
        ccc$Calibration$Analysis$Values$Pressure <-
            setCcl(ccc$Calibration$Analysis$Values$Pressure,
                   "fill",
                   PF$Unit,
                   pfill,
                   msg)
        
        ccc$Calibration$Analysis$Values$Pressure <-
            setCcl(ccc$Calibration$Analysis$Values$Pressure,
                   "fill_offset",
                   PF$Unit,
                   pfilloffset,
                   "pfill is already corrected by fill_offset")
        
    } ## a$cmscok == "opK4"
    
    return(ccc)
}
