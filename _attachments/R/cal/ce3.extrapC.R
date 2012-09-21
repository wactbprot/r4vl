ce3.extrapC <- function(ccc){
  msg <- "calculated by ce3.extrapC"

  a     <-  abbrevList(ccc)
  
  ##      return(list(iLw1 = iLw1,   ## grosser LW
  ##                  iLw2 = iLw2))  ## kleiner LW
  ##  
  ilw   <-  getConductIndex(ccc)
  
  cf <- list()
  
  ## andere Gase kommen noch
  if(a$cmscg == "N2" || a$cmscg == "Ar"){

    gas <- a$cmscg
    
    if(length(ilw$iLw2) > 0){
      cf$a  <-  getConstVal(a$cms, paste("klLw_",gas,"_A", sep=""))
      cf$b  <-  getConstVal(a$cms, paste("klLw_",gas,"_B", sep=""))
      cf$c  <-  getConstVal(a$cms, paste("klLw_",gas,"_C", sep=""))
      cf$d  <-  getConstVal(a$cms, paste("klLw_",gas,"_D", sep=""))
      
    }
    if(length(ilw$iLw1) > 0){
      cf$a  <-  getConstVal(a$cms, paste("grLw_",gas,"_A", sep=""))
      cf$b  <-  getConstVal(a$cms, paste("grLw_",gas,"_B", sep=""))
      cf$c  <-  getConstVal(a$cms, paste("grLw_",gas,"_C", sep=""))
      cf$d  <-  getConstVal(a$cms, paste("grLw_",gas,"_D", sep=""))
      
    }
  }
    
##   ccc$Calibration$Analysis$Values$Conductance <-
##     setCcl(ccc$Calibration$Analysis$Values$Conductance,
##            "cfm3",
##            "l/s",
##            L,
##            msg)
##   
  return(ccc)
}
