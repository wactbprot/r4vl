ce3.newCalPfill <- function(ccc){
  msg   <- "calculated by ce3.newCalPfill"
  
  a      <- abbrevList(ccc)
  pUnit  <- "mbar"
  gas    <- a$cmscg
  ## muss noch aus db kommen
  cdgax001border <- 0.133
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

  poax001 <-  getConstVal(a$cma$Pressure, paste(apre,"x0.01",suf,sep=""))
  poax01  <-  getConstVal(a$cma$Pressure, paste(apre,"x0.1" ,suf,sep=""))
  poax1   <-  getConstVal(a$cma$Pressure, paste(apre,"x1"   ,suf,sep=""))

  pobx001 <-  getConstVal(a$cma$Pressure, paste(bpre,"x0.01",suf,sep=""))
  pobx01  <-  getConstVal(a$cma$Pressure, paste(bpre,"x0.1" ,suf,sep=""))
  pobx1   <-  getConstVal(a$cma$Pressure, paste(bpre,"x1"   ,suf,sep=""))

  pocx001 <-  getConstVal(a$cma$Pressure, paste(cpre,"x0.01",suf,sep=""))
  pocx01  <-  getConstVal(a$cma$Pressure, paste(cpre,"x0.1" ,suf,sep=""))
  pocx1   <-  getConstVal(a$cma$Pressure, paste(cpre,"x1"   ,suf,sep=""))
  
  ## gemessen direkt nach usr-input ok
  PF    <- getSubList(a$cmv, "fill")
  pfill <- getConstVal(NA, NA, PF)

  pfilloffset <- rep(NA,length(pfill))
  
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
      
      pdriftfill[ix1]       <- pdriftfill[ix1]   -  poax1
      pfill[ix1]            <- pfill[ix1]        -  poax1  
     
      pfilloffset[ix1]      <- poax1

    }
    if(length(ix01)>0){
      pdriftfill[ix01]      <- pdriftfill[ix01] -  poax01
      pfill[ix01]           <- pfill[ix01]      -  poax01

      pfilloffset[ix01]     <- poax01

    }
    if(length(ix001)>0){
      pdriftfill[ix001]     <- pdriftfill[ix001]-  poax001
      pfill[ix001]          <- pfill[ix001]     -  poax001
      
      pfilloffset[ix001]    <- poax001
      
    }
    ## Fehlerkorrekturen:
    ## cdga
    ## gasartabh.
    ## F(relativ)=(a+c*pind+e*pind^2)/(1+b*pind+d*pind^2+f*pind^3)
    if(gas == "Ar" || gas = "N2"){
      g <- gas
    }else{
      g <- "N2"
      msg <- paste(msg, "no calibration for CDGA for gas: ", gas". Use N2 instead")
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
      pdriftfill[ix1]     <- pdriftfill[ix1]   -  pobx1
      pfill[ix1]          <- pfill[ix1]        -  pobx1

      pfilloffset[ix1]    <- pobx1
   
    }
    
    if(length(ix01)>0){
      pdriftfill[ix01]    <- pdriftfill[ix01]  -  pobx01
      pfill[ix01]         <- pfill[ix01]       -  pobx01
      
      pfilloffset[ix01]   <- pobx01
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

  return(ccc)
}
