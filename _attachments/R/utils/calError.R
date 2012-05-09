calError <- function(ccc){
  msg <- "calculated by calError()"

  a <- abbrevList(ccc)

  IND <- getSubList(a$cm, "p_ind")
  OFS <- getSubList(a$cm, "p_ind_offset")

  if(is.null(IND)){
    IND <- getSubList(a$cm, "ind")
  }
  
  if(is.null(OFS)){
    OFS <- getSubList(a$cm, "ind_offset")
  }
  
  if(is.null(OFS)){
    OFS <- getSubList(a$cm, "offset")
  }
  
  ofs <-  getConstVal(NA,NA,OFS)
  ind <-  getConstVal(NA,NA,IND)
  
  CAL <- getSubList(a$ca, "cal")
  cal <- getConstVal(NA,NA,CAL)
  
  ## soll auch an SE1 für SRG-Fehler funktionieren
  if(IND$Unit == "DCR" && OFS$Unit == "DCR"){

    d   <- getConstVal(a$cmco1, "d")
    rho <- getConstVal(a$cmco1,"rho" )
    sigma <- getConstVal(a$cmco1,"sigma" )
    
    dcr <- ind - ofs
    
    R <- getConstVal(a$cc, "R" )

    if(a$cs == "SE1"){
      T <- getConstVal(a$cav, "after")
    }
    if(a$cs == "CE3"){
      T <- getConstVal(a$cav, "Tuhv")
    }
    if(  a$cmscg == "Ar"){
      M <- getConstVal(a$cc, "molWeight_Ar" )
      msg <- paste(msg, "; gas:", a$cmscg)
    }
    if(  a$cmscg == "N2"){
      M <- getConstVal(a$cc, "molWeight_N2" )
      msg <- paste(msg, "; gas:", a$cmscg)
    }

    
    if(CAL$Unit == "mbar"){
      
      K <- sqrt(8*R*(T)/(pi*M))*pi*d*rho/2000
      
      corrind  <- K * dcr/sigma
      ind      <- K * ind
      ofs      <- K * ofs

    }else{
      stop("conversion to mbar needed; todo!")
    }
    
  }else{
    
    convInd <- getConvFactor(ccc, CAL,IND)
    convOfs <- getConvFactor(ccc, CAL,OFS)
    
    ind <- getConstVal(NA,NA,IND) * convInd
    ofs <- getConstVal(NA,NA,OFS) * convOfs

    corrind <- (ind - ofs)
    
    ## unit of ind and offset is now
    indUnit <- CAL$Unit
    
    msg <- paste(msg,
                 "ind conversion factor was:", convInd,
                 "offset conversion factor was:",convOfs )
    
  }
    




  error <- corrind/cal - 1

  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure, "ind",
           indUnit,
           ind,
           paste(msg)
           )

  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure, "offset",
           indUnit,
           ofs,
           paste(msg)
           )

  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure, "corrind",
           indUnit,
           corrind,
           paste(msg, ";p_ind - p_ind_offset")
           )

  ccc$Calibration$Analysis$Values$Error <-
    setCcl(ccc$Calibration$Analysis$Values$Error, "relative",
           "1",
           error,
           paste(msg, "error is defined by (corrind/p_cal-1)")
           )

  return(ccc)
}
