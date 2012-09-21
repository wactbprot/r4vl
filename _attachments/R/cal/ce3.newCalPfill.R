ce3.newCalPfill <- function(ccc){
  msg   <- "calculated by ce3.newCalPfill"
  pUnit <- "mbar"

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
  
  a     <- abbrevList(ccc)

  apre <- "cdga_"
  bpre <- "cdgb_"
  cpre <- "cdgc_"
  suf  <- "_offset"

  poax001 <-  getConstVal(a$cma$Pressure, paste(apre,"x0.01",suf,sep=""))
  poax01  <-  getConstVal(a$cma$Pressure, paste(apre,"x0.1",suf,sep=""))
  poax1   <-  getConstVal(a$cma$Pressure, paste(apre,"x1",suf,sep=""))

  pobx001 <-  getConstVal(a$cma$Pressure, paste(bpre,"x0.01",suf,sep=""))
  pobx01  <-  getConstVal(a$cma$Pressure, paste(bpre,"x0.1",suf,sep=""))
  pobx1   <-  getConstVal(a$cma$Pressure, paste(bpre,"x1",suf,sep=""))
  
  ## gemessen direkt nach usr-input ok
  PF    <- getSubList(a$cmv, "fill")
  pfill <- getConstVal(NA, NA, PF)

  ## haben die gleiche Unit wie pfill
  bdf <- getConstVal(a$cmv, "before_drift_fill")
  adf <- getConstVal(a$cmv, "after_drift_fill")
  
  pdrift <-mean(c(mean(bdf),mean(adf)))

  icdga <- which(pdrift <= cdgax1border)
  icdgb <- which(pdrift > cdgbx001border) 

  if(length(icdga > 0){

    pda <- pdrift[icdga]
    pfa <- pfill[icdga]
    
    ix001 <- which(pda < cdgax001border)
    ix01  <- which(pda < cdgax01border)
    ix1   <- which(pda < cdgax1border)
    
    if(length(ix1)>0){
      pda[ix1] <- pda[ix1] -  poax1
      pfa[ix1] <- pfa[ix1] -  poax1  
    }
    if(length(ix01)>0){
      pda[ix01] <- pda[ix01] -  poax01
      pfa[ix01] <- pfa[ix01] -  poax01  
    }
    if(length(ix01)>0){
      pda[ix001] <- pda[ix001] -  poax001
      pfa[ix001] <- pfa[ix001] -  poax001  
    }

    pdrift[icdga] <- pda
    pfill[icdga]  <- pfa
  }# cdga

  if(length(icdgb > 0){

    pdb <- pdrift[icdgb]
    pfb <- pfill[icdgb]
    
    ix001 <- which(pdb < cdgbx001border)
    ix01  <- which(pdb < cdgbx01border)
    ix1   <- which(pdb < cdgbx1border)
    
    if(length(ix1)>0){
      pdb[ix1] <- pdb[ix1] -  pobx1
      pfb[ix1] <- pfb[ix1] -  pobx1  
    }
    if(length(ix01)>0){
      pdb[ix01] <- pdb[ix01] -  pobx01
      pfb[ix01] <- pfb[ix01] -  pobx01  
    }
    if(length(ix01)>0){
      pdb[ix001] <- pdb[ix001] -  pobx001
      pfb[ix001] <- pfb[ix001] -  pobx001  
    }

    pdrift[icdgb] <- pdb
    pfill[icdgb]  <- pfb
  }# cdgb

    
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


  
  return(ccc)
}
