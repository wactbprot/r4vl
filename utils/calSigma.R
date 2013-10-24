calSigma <- function( ccc ){

  msg <- "calculated by calSigma()"
  a   <- abbrevList(ccc)

  ## ab welchem Druck sollen die gemessenen  sigma- werte auch extrapoliert werden:
  extrLowerValue <- 1.0e-3
  extrLowerUnit  <- "mbar"
  

  ## neues Problem: welches sigma soll hier gerechnet werden?
  ## Antwort: das cmco1 (also das "Customer CO")

  d   <- getConstVal(a$cmco1, "d")
  rho <- getConstVal(a$cmco1,"rho" )

  RD  <- getSubList(a$cmv,  "p_ind_offset")
  if(is.null(RD)){
    RD <- getSubList(a$cmv,  "offset")
  }

  IND <- getSubList(a$cmv, "p_ind" )
  if(is.null(IND)){
    IND <- getSubList(a$cmv,  "ind")
  }

  rd <-  getConstVal(NA,NA, RD)
  ind <- getConstVal(NA,NA,IND)

  ## eigentlich gehöhrt hier auch mal 'ne function geschrieben ...
  ## ... done:

  ## seit 7.2.11 gibt es in getOutIndex
  ## eine pind - off Klausel
  ## ist seit Okt. 11 nicht mehr nötig

  CAL <- getSubList(a$cav$Pressure, "cal" )
  ## check unit of CAL
  cal <- getConstVal(NA,NA,CAL)


  if(length(grep("mbar",IND$Unit)) >0){

    corrind <- ind - rd
  }

  if((IND$Unit == "DCR") & (CAL$Unit == "mbar")){
    dcr <- ind - rd
    R <- getConstVal(a$cc, "R" )
    if(a$cs == "SE1"){
      T <- getConstVal(a$cav, "after")
    }
    if(a$cs == "CE3"){
      T <- getConstVal(a$cav, "Tuhv")
    }
    if(  a$cmag == "Ar"){
      M <- getConstVal(a$cc, "molWeight_Ar" )
      msg <- paste(msg, "; a$cmag:", a$cmag)
    }
    if(  a$cmag == "N2"){
      M <- getConstVal(a$cc, "molWeight_N2" )
      msg <- paste(msg, "; gas:", a$cmag)
    }
    if(  a$cmag == "D2"){
      M <- getConstVal(a$cc, "molWeight_D2" )
      msg <- paste(msg, "; gas:", a$cmag)
    }

    IndUnit <- CAL$Unit
    K <- sqrt(8*R*(T)/(pi*M))*pi*d*rho/2000

    corrind <- K * dcr
    ind     <- K * ind
    rd      <- K * rd

    ## sind daten zum extrapolieren des sigma da?:
    compCal <- cal * getConvFactor(ccc,extrLowerUnit,CAL$Unit)
    iextr   <- which(compCal > extrLowerValue ) # mbar
    extrok  <- length(iextr) > 3
    sigma <- corrind/cal
    ## hier kein Out Index mehr,
    ## da alle inputs schon über oi gelaufen sind

    sigma0 <- NA

    if(extrok){

      sigma0 <- as.vector(coefficients(lm(sigma[iextr] ~cal[iextr])))[1]

    }


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
             paste(msg, "calculated with sqrt(8*R*(T)/(pi*M))*pi*d*rho/2000*(dcr-rd)"))

    ccc$Calibration$Analysis$Values$Sigma <-
      setCcl(ccc$Calibration$Analysis$Values$Sigma, "eff",
             paste(IndUnit,"/",CAL$Unit,  sep=""),
             sigma,
             msg)

    ccc$Calibration$Result$Values$Sigma <-
      setCcl(ccc$Calibration$Result$Values$Sigma, "0",
             "1",
             sigma0,
             paste(msg,"points: ",toString(iextr), "used for extrapolation") )
    
  }
  
  return( ccc )
}
