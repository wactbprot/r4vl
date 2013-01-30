se1.yamp.calT <- function(ccc) {
  msg <- "calculated by yamp.calT"
  
  a <- abbrevList( ccc )
  
  if(a$dataAvailable){
    
    ## das:
    TUnit <- "K"
    C2K <- getConstVal(a$cc, "C_2_K")
    ## kann man später noch über
    ## getConvFactor()
    ## abwickeln
    
    ## a$cmscex steht für SequenceControll/Expansion
    N <- length(getConstVal(a$cmv, "amt_before"))
    
    baseCorrName        <- "keithley_corr_ch" 
    baseTNameAfter      <- "keithley_T_after_ch"
    baseTNameRoom       <- "keithley_T_room_ch"
    baseTNameBefore     <- "keithley_T_before_ch" 
    expansionBaseName   <- "Expansion_" ## 

    ## T_after ---------------------------------v
    channelsRoom <- c("110")
    roomRes <- getTemperatureVec(a,N,channelsRoom,baseTNameRoom,baseCorrName) + C2K
    ## T_after ---------------------------------^

    
    ## T_after ---------------------------------v
    channelsAfter <- c("103","104","105","106","107","108")
    afterRes <- getTemperatureVec(a,N,channelsAfter,baseTNameAfter,baseCorrName) + C2K
    ## T_after ---------------------------------^
    
    ## T_before ---------------------------------v
    exNames <- getSubList(a$cmv, "name")$Value

    iA <- which(exNames == paste(expansionBaseName, "A", sep=""))
    iB <- which(exNames == paste(expansionBaseName, "B", sep=""))
    iC <- which(exNames == paste(expansionBaseName, "C", sep=""))
    iD <- which(exNames == paste(expansionBaseName, "D", sep=""))
    iE <- which(exNames == paste(expansionBaseName, "E", sep=""))
    
    resBefore <- rep(NA,N)
    
    if(length(iA)>0){
      channelsBefore <- c("101") 
      resBefore[iA]<- getTemperatureVec(a,N,channelsBefore,baseTNameBefore,baseCorrName)[iA] + C2K
    }
    
    if(length(iB)>0){
      channelsBefore <- c("102") 
      resBefore[iB]<- getTemperatureVec(a,N,channelsBefore,baseTNameBefore,baseCorrName)[iB] + C2K
    }
    
    if(length(iC)>0){
      channelsBefore <- c("102") 
      resBefore[iC]<- getTemperatureVec(a,N,channelsBefore,baseTNameBefore,baseCorrName)[iC] + C2K
    }
    
    if(length(iD)>0){
      channelsBefore <- c("102") 
      resBefore[iD]<- getTemperatureVec(a,N,channelsBefore,baseTNameBefore,baseCorrName)[iD] + C2K
    }
    
    if(length(iE)>0){
      channelsBefore <- c("109") 
      resBefore[iE]<- getTemperatureVec(a,N,channelsBefore,baseTNameBefore,baseCorrName)[iE] + C2K
    }
    
    ## T_before ---------------------------------^  

    resBefore <- checkOutIndex(a, resBefore)
    afterRes  <- checkOutIndex(a, afterRes)
    
    ccc$Calibration$Analysis$Values$Temperature <-
      setCcl(ccc$Calibration$Analysis$Values$Temperature,
             "before",
             TUnit,
             resBefore,
             msg)
    
    ccc$Calibration$Analysis$Values$Temperature <-
      setCcl(ccc$Calibration$Analysis$Values$Temperature,
             "after",
             TUnit,
             afterRes,
             msg)
    
    ccc$Calibration$Analysis$Values$Temperature <-
      setCcl(ccc$Calibration$Analysis$Values$Temperature,
             "room",
             TUnit,
             roomRes,
             paste(msg, "room is ch", channelsRoom,"at",baseTNameRoom))
    
    
    return(ccc)
  }
}
