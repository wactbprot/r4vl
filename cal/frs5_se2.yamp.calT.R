frs5_se2.yamp.calT <- function(ccc){
  msg  <- "calculated by frs5_se2.yamp.calT()"
  a    <- abbrevList(ccc)

  TUnit   <- "K"
  C2K     <- getConstVal(a$cc, "C_2_K")

  exName  <- getSubList(a$cmv$Expansion, "name")$Value 
  if1     <- which(exName == "f1")
  if2     <- which(exName == "f2")
  if3     <- which(exName == "f3")
  if4     <- which(exName == "f4")
  if5     <- which(exName == "f5")
  

### ============================== Keithley =============================

  ##  T_frs ---------------------------------v
  frs5Tch             <- "203"
  frs5TName           <-  paste("keithley_T_ch", frs5Tch, sep="")
  frs5TCorrName       <-  paste("keithley_corr_ch",frs5Tch,sep="")
  TFRS                <-  getSubList(a$cmv$Temperature,frs5TName)
  corrTFrs5           <-  getConstVal(a$cmco,frs5TCorrName)
  Tfrs                <-  getConstVal(NA,NA,TFRS)  + corrTFrs5
  ##  T_frs ---------------------------------^

  N                   <-  length( Tfrs )

  resBeforeAgile      <- rep(NA, N)
  resBeforeKeith      <- rep(NA, N)

  
  baseTNameAfter      <- "keithley_T_after_ch"
  baseTNameBefore     <- "keithley_T_before_ch" 
  baseCorrName        <- "keithley_corr_ch" 
  
  
  ## T_after ---------------------------------v
  channelsAfter <- c("105","106","107","108","109","110")
  resAfterKeith <- getTemperatureVec(a,N,channelsAfter,baseTNameAfter,baseCorrName) + C2K
  ## T_after ---------------------------------^

  
  ## T_before ---------------------------------v
  if(length(if1) > 0){
      channelsBefore <- c("102", "103")
      resBeforeKeith[if1] <- getTemperatureVec(a,N,channelsBefore,baseTNameBefore,baseCorrName)[if1] + C2K
  }
  if(length(if2) > 0){
      channelsBefore <- c("101")
      resBeforeKeith[if2] <- getTemperatureVec(a,N,channelsBefore,baseTNameBefore,baseCorrName)[if2] + C2K
  }
  if(length(if5) > 0){
      channelsBefore <- c("104")
      resBeforeKeith[if5] <- getTemperatureVec(a,N,channelsBefore,baseTNameBefore,baseCorrName)[if5] + C2K
  }
  if(length(if3) > 0){
      channelsBefore <- c("102", "103")
      resBeforeKeith[if3] <- getTemperatureVec(a,N,channelsBefore,baseTNameBefore,baseCorrName)[if3] + C2K
  }
  if(length(if4) > 0){
      channelsBefore <- c("101")
      resBeforeKeith[if4] <- getTemperatureVec(a,N,channelsBefore,baseTNameBefore,baseCorrName)[if4] + C2K
  }
  ## T_before ---------------------------------^

### ============================== Agilent =============================


  baseTName      <- "agilentCh"
  baseCorrName   <- "agilentCorrCh" 
  
  
  ## T_after ---------------------------------v
  channelsAfter <- c("105","106","107","108","109","110")
  resAfterAgile <- getTemperatureVec(a,N,channelsAfter,baseTName,baseCorrName,"_after") + C2K
  ## T_after ---------------------------------^
  
  
  ## T_before ---------------------------------v
  if(length(if1) > 0){
      channelsBefore <- c("102", "103")
      resBeforeAgile[if1] <- getTemperatureVec(a,N,channelsBefore,baseTName,baseCorrName, "_before")[if1] + C2K
  }
  if(length(if2) > 0){
      channelsBefore <- c("101")
      resBeforeAgile[if2] <- getTemperatureVec(a,N,channelsBefore,baseTName,baseCorrName, "_before")[if2] + C2K
  }
  if(length(if5) > 0){
      channelsBefore <- c("104")
      resBeforeAgile[if5] <- getTemperatureVec(a,N,channelsBefore,baseTName,baseCorrName, "_before")[if5] + C2K
  }
  if(length(if3) > 0){
      channelsBefore <- c("102", "103")
      resBeforeAgile[if3] <- getTemperatureVec(a,N,channelsBefore,baseTName,baseCorrName, "_before")[if3] + C2K
  }
  if(length(if4) > 0){
      channelsBefore <- c("101")
      resBeforeAgile[if4] <- getTemperatureVec(a,N,channelsBefore,baseTName,baseCorrName, "_before")[if4] + C2K
  }
  ## T_before ---------------------------------^
  
  ccc$Calibration$Analysis$Values$Temperature <-
    setCcl(ccc$Calibration$Analysis$Values$Temperature,
           "frs5",
           "C",
           Tfrs,
           paste(msg,"corrected with: ", corrTFrs5 ))
    
    ccc$Calibration$Analysis$Values$Temperature <-
      setCcl(ccc$Calibration$Analysis$Values$Temperature,
             "before",
             TUnit,
             rowMeans(cbind(resBeforeKeith,resBeforeAgile)),
             msg)
    
    ccc$Calibration$Analysis$Values$Temperature <-
      setCcl(ccc$Calibration$Analysis$Values$Temperature,
             "after",
             TUnit,
             rowMeans(cbind(resAfterKeith,resAfterAgile)),
             msg)
  return(ccc)
}
