se1.calRGC <- function(ccc){
  msg <- "calculated by calRGC() (RGC ... real gas correction)"
  
  a <- abbrevList( ccc )
  
  PFILL <- getSubList(a$cav$Pressure, "fill")
  pfill <- getConstVal(NA, NA, PFILL)
  
  gas   <-  a$cpt$Gas
  VC    <-  getSubList(a$cc,paste("virialCoeff_", gas, sep=""))  
  SP    <-  getSubList(a$cc, "standardPressure")
  SV    <-  getSubList(a$cc, "standardVolumen" )
  
  
  if((VC$Unit == "cm^3/mol") & (SV$Unit == "cm^3")){

    conv <- getConvFactor(ccc,PFILL,SP) 
    msg  <- paste(msg, "converted", SP$Unit, "to", PFILL$Unit, "with", conv)
    
    sp   <- getConstVal(NA,NA,SP) * conv
    
    sv   <- getConstVal(NA,NA,SV)
    vc   <- getConstVal(NA,NA,VC)
    
    ## der outIndex muss nicht kontrolliert werden,
    ## weil nur pfill vector ist und dieser aus Analysis kommt,
    ## also schon über getOutIndex() gelaufen ist
    
    rgc  <-  (vc/(sv * sp) *  pfill)

    msg  <- paste(msg,"1- Correction_rg") 

  }else{
    stop(
         paste("don't know how to handle",
               VG$Unit,
               "in virialCoeff  with",
               SV$Unit,
               "in in standardVolumen")
         )
  }
  
 ccc$Calibration$Analysis$Values$Correction <-
      setCcl(ccc$Calibration$Analysis$Values$Correction,
             "rg",
             "1",
             rgc,
             msg)
    
  return(ccc)
}

