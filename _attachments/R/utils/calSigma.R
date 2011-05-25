calSigma <- function( ccc ){
  
  msg <- "calculated by calSigma()"

  a <- abbrevList(ccc)

  
  ## im neuen jQuery manCust müssen die values
  ## bereits als float geschrieben werden

  ## neues Problem: welches sigma soll hier gerechnet werden?
  ## Antwort: das cmco1 (also das "Customer CO")
  ## besser ist das Einführen eines TypePrefix
  ## unter CalibrationObject.Device
  ## es kann so der Name erstellt werden:
  

  d <- getConstVal(a$cmco1, "d")
  rho <- getConstVal(a$cmco1,"rho" )

  RD <- getSubList(a$cmv,  "p_ind_offset")
  IND <- getSubList(a$cmv, "p_ind" )
 
  rd <-  getConstVal(NA,NA, RD)
  ind <- getConstVal(NA,NA,IND)
 
  ## eigentlich gehöhrt hier auch mal 'ne function geschrieben ...
  ## ... done:

  ## seit 7.2.11 gibt es in getOutIndex
  ## eine pind - off Klausel
  rd  <- checkOutIndex(a,rd)
  ind <- checkOutIndex(a,ind)
  
  CAL <- getSubList(a$cav$Pressure, "cal" )
  ## check unit of CAL
  cal <- getConstVal(NA,NA,CAL)

 
  if(length(grep("mbar",IND$Unit)) >0){

    corrind <- ind - rd
  }
  
  if(IND$Unit == "DCR"){

    dcr <- ind - rd

    R <- getConstVal(a$cc, "R" )

    if(a$cs == "SE1"){
      T <- getConstVal(a$cav, "after")
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
      IndUnit <- "mbar"
   
      K <- sqrt(8*R*(T)/(pi*M))*pi*d*rho/2000

      corrind <- K * dcr
      ind     <- K * ind
      rd      <- K * rd
      
      msg     <- paste(msg, "calculated with sqrt(8*R*(T)/(pi*M))*pi*d*rho/2000*(dcr-rd)")
    }else{
      stop(paste(CAL$Unit, "not implemented as unit for pcal"))
    }
  }## Unit == DCR

  if(length(grep(CAL$Unit,IndUnit)) >0){

    sigma <- corrind/cal
    ## hier kein Out Index mehr,
    ## da alle inputs schon über oi gelaufen sind

    ccc$Calibration$Analysis$Values$Pressure <-
      setCcl(ccc$Calibration$Analysis$Values$Pressure, "ind",
             IndUnit,
             ind,
             msg)
    
    ccc$Calibration$Analysis$Values$Pressure <-
      setCcl(ccc$Calibration$Analysis$Values$Pressure, "ind_offset",
             IndUnit,
             rd,
             msg)
    
    ccc$Calibration$Analysis$Values$Pressure <-
      setCcl(ccc$Calibration$Analysis$Values$Pressure, "ind_corr",
             IndUnit,
             corrind,
             msg)
    
    ccc$Calibration$Analysis$Values$Sigma <-
      setCcl(ccc$Calibration$Analysis$Values$Sigma, "eff",
             paste(IndUnit,"/",CAL$Unit,  sep=""),
             sigma,
             msg)
    
  }else{
    stop("cal and ind Units dont match")
  }
  
  return( ccc )
}
