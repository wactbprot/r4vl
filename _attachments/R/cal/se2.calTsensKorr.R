se2.calTsensKorr <- function(ccc){
  msg <- "calculated by se2.calTsensKorr"
  
  a    <- abbrevList( ccc )
  maxT <- 24
  minT <- 22
  
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

    nout <- which(is.na(nVal) | (nVal < minT) | (nVal > maxT))

    nVal <- nVal[-nout]
       
    
    for(idx in 1:length(chVec)){

      cmplName <- paste(baseName, chVec[idx], sep="")

      iVal     <-  getConstVal(a$cmv,cmplName)
      iout     <-  which((is.na(nVal) | (iVal < minT) | (iVal > maxT)))
      iVal     <-  iVal[-c(nout,iout)]
      
      
      if(length(iVal) > length(nVal)){
        
       
        kVal     <- nVal - iVal[1:length(nVal)]
      }else{

        kVal     <- nVal[1:length(iVal)] - iVal

      }


      k        <- mean(kVal, na.rm = TRUE)
      sdK      <- sd(kVal, na.rm = TRUE)
      
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
