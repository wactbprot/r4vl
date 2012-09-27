ce3.newCalPfill <- function(ccc){
  msg   <- "calculated by ce3.newCalPfill"
  
  a      <- abbrevList(ccc)
  pUnit  <- "mbar"
  gas    <- a$cmscg
  ## muss noch aus db kommen
  cdgax001border <- 0.133
  cdgax01border  <- 1.33
  cdgax1border   <- 13.3
  
  cdgbx001border <- 13.3
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

  ## zur Korr. der Druckdiff. beim Schließen des V22
  ## (dpC wird nur in X.1 Range gemessen
  dpC <- getConstVal(a$cmv,"drift_mean_p") - pocx01

  ## haben die gleiche Unit wie pfill
  bdf <- getConstVal(a$cmv, "before_drift_fill")
  adf <- getConstVal(a$cmv, "after_drift_fill")
  
  pdrift <- (bdf + adf)/2

  icdga <- which(pdrift <= cdgax1border)
  icdgb <- which(pdrift > cdgbx001border) 

  if(length(icdga > 0)){
    msg <- paste(msg,"assume cdga for points:", toString(icdga))
    
    ix001 <- which(pdrift < cdgax001border)
    ix01  <- which(pdrift < cdgax01border && pdrift > cdgax001border)
    ix1   <- which(pdrift < cdgax1border  && pdrift > cdgax01border)
    
    if(length(ix1)>0){
      pdrift[ix1]       <- pdrift[ix1]   -  poax1
      pfill[ix1]        <- pfill[ix1]    -  poax1  

      msg <- paste(msg,
                   "offset:",
                   poax1,
                   "subtracted from points:",
                   toString(ix1))
    }
    if(length(ix01)>0){
      pdrift[ix01]       <- pdrift[ix01] -  poax01
      pfill[ix01]        <- pfill[ix01]  -  poax01

      msg <- paste(msg,
                   "offset:",
                   poax01,
                   "subtracted from points:",
                   toString(ix01))
    }
    if(length(ix001)>0){
      pdrift[ix001]      <- pdrift[ix001]-  poax001
      pfill[ix001]       <- pfill[ix001] -  poax001

      msg <- paste(msg,
                   "offset:",
                   poax001,
                   "subtracted from points:",
                   toString(ix001))
    }
    ## Fehlerkorrekturen:
    ## cdga
    ## gasartabh.
    ## F(relativ)=(a+c*pind+e*pind^2)/(1+b*pind+d*pind^2+f*pind^3)
    cfcdga    <- list()
    cfcdga$a  <- getConstVal(  a$cmco, paste("cdgaCorrA_",gas,sep=""))
    cfcdga$b  <- getConstVal(  a$cmco, paste("cdgaCorrB_",gas,sep=""))
    cfcdga$c  <- getConstVal(  a$cmco, paste("cdgaCorrC_",gas,sep=""))
    cfcdga$d  <- getConstVal(  a$cmco, paste("cdgaCorrD_",gas,sep=""))
    cfcdga$e  <- getConstVal(  a$cmco, paste("cdgaCorrE_",gas,sep=""))
    cfcdga$f  <- getConstVal(  a$cmco, paste("cdgaCorrF_",gas,sep=""))
       
    pdrift[icdga] <- pdrift[icdga]/(fn.7904(cfcdga,pdrift[icdga]) + 1)
    pfill[icdga]  <- pfill[icdga]/(fn.7904(cfcdga,pfill[icdga]) + 1)

  }# cdga

  if(length(icdgb > 0)){
    msg <- paste(msg,"assume cdgb for points:", toString(icdgb))
    ix001 <- which(pdrift < cdgbx001border)
    ix01  <- which(pdrift < cdgbx01border && pdrift > cdgbx001border)
    ix1   <- which(pdrift < cdgbx1border  && pdrift > cdgbx01border)
    
    if(length(ix1)>0){
      pdrift[ix1]     <- pdrift[ix1]   -  pobx1
      pfill[ix1]      <- pfill[ix1]    -  pobx1
      msg <- paste(msg,
                   "offset:",
                   pobx1,
                   "subtracted from points:",
                   toString(ix1))
    }
    if(length(ix01)>0){
      pdrift[ix01]   <- pdrift[ix01]   -  pobx01
      pfill[ix01]    <- pfill[ix01]    -  pobx01
      msg <- paste(msg,
                   "offset:",
                   pobx01,
                   "subtracted from points:",
                   toString(ix01))
    }
    
    if(length(ix001)>0){
      pdrift[ix001] <- pdrift[ix001]   -  pobx001
      pfill[ix001]  <- pfill[ix001]    -  pobx001  
      msg <- paste(msg,
                   "offset:",
                   pobx001,
                   "subtracted from points:",
                   toString(ix001))
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
        
    pdrift[icdgb] <- pdrift[icdgb]/(fn.7904(cfcdgb,pdrift[icdgb]) + 1) 
    pfill[icdgb]  <- pfill[icdgb]/(fn.7904(cfcdgb,pfill[icdgb]) + 1)
    
  }# cdgb

  ## zum Fülldruck der Leitwertmessung
  ## kommt noch die Druckdifferenz
  ## durch das Schließen des V22
  ## bei pfill ist das nicht dabei,
  ## weil hier V22 offen ist
  pdrift <- pdrift + dpC

  
  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           "lw",
           PF$Unit,
           pdrift,
           msg)
    
  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           "fill",
           PF$Unit,
           pfill,
           msg)
  
  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           "dpC",
           PF$Unit,
           dpC,
           msg)
  return(ccc)
}
