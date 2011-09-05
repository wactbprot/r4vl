se2.calTsensKorr <- function(ccc){
  msg <- "calculated by se2.calTsensKorr"
  
  a <- abbrevList( ccc )
  
  if(a$dataAvailable){
    baseName <- "keithley_ch"
    chVec    <- c("101",
                  "102",
                  "103",
                  "104",
                  "105",
                  "106",
                  "107",
                  "108",
                  "109",
                  "110",
                  "201",
                  "202",
                  "203",
                  "204")
    
    normName <- "f250"
 
    nVal <-  getConstVal(a$cmv,normName)
    nout <- which(is.na(nVal))

    for(idx in 1:length(chVec)){

      cmplName <- paste(baseName, chVec[idx], sep="")

      iVal     <-  getConstVal(a$cmv,cmplName)
      iout     <-  which(is.na(iVal))      
      iVal     <-  iVal[-c(nout,iout)]
       
      kVal     <- nVal[-c(nout,iout)] - iVal
      k        <- mean(kVal)
      sdK      <- sd(kVal)
      
      ccc$Calibration$Result$Values$Temperature <-
        setCcl(ccc$Calibration$Result$Values$Temperature,
               paste("keithley_corr_ch",chVec[idx],sep=""),
               "K",
               k,
               paste(msg,"mean(T_ptb_j - T_chi_j) mit T_chi:",cmplName ))
      
      ccc$Calibration$Result$Values$Temperature <-
        setCcl(ccc$Calibration$Result$Values$Temperature,
               paste("keithley_sd_corr_ch",chVec[idx],sep=""),
               "K",
               sdK,
               paste(msg,"sd(T_ptb_j - T_chi_j) mit T_chi:",cmplName ))
    }
  }
  return(ccc)
}
