ce3.calTsensKorr <- function(ccc){
  msg <- "calculated by ce3.calTsensKorr"
  
  a <- abbrevList( ccc )
  
  if(a$dataAvailable){
    baseName <- "agilentCh"
    chVec    <- c("101",
                  "102",
                  "103",
                  "104",
                  "105",
                  "106",
                  "107",
                  "108",
                  "109",
                  "110")
    
    normName <- "f250"
 
    nVal <-  getConstVal(a$cmv,normName)
    nout <- which(is.na(nVal))

    for(idx in 1:length(chVec)){

      cmplName <- paste(baseName, chVec[idx], sep="")

      iVal     <-  getConstVal(a$cmv,cmplName)
    
      iout     <-  which(is.na(iVal))      
      ## wenn alles funk. hat, also  nout und iout die Länge 0 
      ## (integer(0)) haben, funk.   [-c(nout,iout)] nicht.
      if(length(nout) > 0 || length(iout) > 0 ){
        kVal     <- nVal[-c(nout,iout)] - iVal[-c(nout,iout)]
      }else{
        kVal     <- nVal - iVal
      }
      
      k        <- mean(kVal)
      sdK      <- sd(kVal)
      
      ccc$Calibration$Result$Values$Temperature <-
        setCcl(ccc$Calibration$Result$Values$Temperature,
               paste("agilentCorrCh",chVec[idx],sep=""),
               "K",
               k,
               paste(msg,"mean(T_ptb_j - T_chi_j) mit T_chi:",cmplName ))
      
      ccc$Calibration$Result$Values$Temperature <-
        setCcl(ccc$Calibration$Result$Values$Temperature,
               paste("agilentSdCorrCh",chVec[idx],sep=""),
               "K",
               sdK,
               paste(msg,"sd(T_ptb_j - T_chi_j) mit T_chi:",cmplName ))
    }
  }
  return(ccc)
}
