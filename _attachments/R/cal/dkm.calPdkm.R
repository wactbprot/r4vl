dkm.calPdkm <- function(ccc){
  msg <- "calculated by  dkm.calPcal()"
  a <- abbrevList(ccc)

  calUnit   <- "Pa"
  TUnit     <- "C"

  maxDev    <- 100 ## maximal zulässige Abweichung  pdkm - pnom in Pa

  PNOM      <- getSubList(a$cmv$Pressure,    "dkm_nom")
  PRES      <- getSubList(a$cmv$Pressure,    "dkm_res")
  TDKM      <- getSubList(a$cmv$Temperature, "keithley_ch108")

  SYSTCHAN  <-  getSubList(a$cms,"system_change")


  convNom   <- getConvFactor(ccc,calUnit,PNOM$Unit)
  convRes   <- getConvFactor(ccc,calUnit,PRES$Unit)
  convSC    <- getConvFactor(ccc,calUnit,SYSTCHAN$Unit)
  convT     <- getConvFactor(ccc,TUnit,TDKM$Unit)

  g         <- getConstVal(a$cc,"g")
  alphaBeta <- getConstVal(a$cms,"alpha_beta_dkm")


  pnom        <- getConstVal(NA,NA,PNOM)    * convNom  # ab hier Pa
  pres        <- getConstVal(NA,NA,PRES)    * convRes  # Pa
  Tdkm        <- getConstVal(NA,NA,TDKM)    * convT  # C
  systChange  <- getConstVal(NA,NA,SYSTCHAN)* convSC   # auch in Pa

  mSystVec    <- rep(NA, length(pnom))
  Avec        <- rep(NA, length(pnom))
  mpieceVals  <- rep(NA, length(pnom))

  ASyst1      <- getConstVal(a$cms,"A_system_1")
  ASyst2      <- getConstVal(a$cms,"A_system_2")

  mSyst1      <- getConstVal(a$cms,"m_system_1")
  mSyst2      <- getConstVal(a$cms,"m_system_2")

  mNamesL <- c("m_31","m_32","m_33","m_34","m_35","m_36","m_37","m_38","m_39")
  mNamesM <- c("m_41","m_42")
  mNamesS <- c("m_61","m_62","m_63","m_64","m_65","m_66","m_67","m_68","m_69")

  ip1     <- which(pnom <= systChange) # system1 bis 300mbar
  ip2     <- which(pnom >  systChange) # system2 ab 300mbar

  if(length(ip1) >0){
    mSystVec[ip1] <- mSyst1
    Avec[ip1] <- ASyst1
  }

  if(length(ip2) >0){
    mSystVec[ip2] <- mSyst2
    Avec[ip2] <- ASyst2
  }

  noOfPnom <- length(pnom)

  pdkm <- rep(0,noOfPnom)
  mName <- rep(0,noOfPnom)

  ##
  for(currPoint in 1:noOfPnom){

    ## Namen (Bezeichn.)
    sumNamesS <- gsub("m_","sum_to_",mNamesS)
    sumNamesM <- gsub("m_","sum_to_",mNamesM)
    sumNamesL <- gsub("m_","sum_to_",mNamesL)
    namesCompSM   <- outer(sumNamesS,sumNamesM,"paste")
    namesCompSL   <- outer(sumNamesS,sumNamesL,"paste")
    namesCompML   <- outer(sumNamesM,sumNamesL,"paste")

    ## Massen
    mSumS <- sumMpieces(a$cms,mNamesS)
    mSumM <- sumMpieces(a$cms,mNamesM)
    mSumL <- sumMpieces(a$cms,mNamesL)
    mCompSM   <- outer(mSumS,mSumM,"+")
    mCompSL   <- outer(mSumS,mSumL,"+")
    mCompML   <- outer(mSumM,mSumL,"+")

    ## Drücke (in Pa)
    pSumS <-  (mSumS + mSystVec[currPoint])*g/Avec[currPoint]
    pSumM <-  (mSumM + mSystVec[currPoint])*g/Avec[currPoint]
    pSumL <-  (mSumL + mSystVec[currPoint])*g/Avec[currPoint]
    pCompSM   <-  (mCompSM + mSystVec[currPoint])*g/Avec[currPoint]
    pCompSL   <-  (mCompSL + mSystVec[currPoint])*g/Avec[currPoint]
    pCompML   <-  (mCompML + mSystVec[currPoint])*g/Avec[currPoint]

    ## alles in gleicher Weise zu einem Vektor
    deltaPall <- c(pSumS,pSumM,pSumL,
                   as.vector(pCompSM),
                   as.vector(pCompSL),
                   as.vector(pCompML)) - pnom[currPoint]

    namesAll <- c(sumNamesS,sumNamesM,sumNamesL,
                  as.vector(namesCompSM),
                  as.vector(namesCompSL),
                  as.vector(namesCompML))

    mAll <- c(mSumS,mSumM,mSumL,
              as.vector(mCompSM),
              as.vector(mCompSL),
              as.vector(mCompML))

    ## Abweichungen < maxDev und > 0

    ip <-  which((deltaPall < maxDev) &(deltaPall > 0) )

    ## für den Fall, dass mehr als eine Möglichkeit exist. wird die erste genommen
    if(length(ip) == 0){
      sumM <- 0
      msg <- paste(msg,
                   "no mass or mass combination is",
                   "better than 100Pa related to",
                   "a nominal pressure of :",
                   pnom[currPoint])
    }
    if(length(ip) == 1){
      sumM <- mAll[ip]
    }
    if(length(ip) > 1){
      sumM <- mAll[ip[1]]
      msg <- paste(msg,
                   "at point: ",
                   currPoint,
                   "exist more than one possibility:",
                   namesAll[ip],
                   "take: ",namesAll[ip[1]])
    }

    ## mName[currPoint] <- namesAll[ip]
    pdkm[currPoint]  <- g*(sumM+mSystVec[currPoint])/
                        (Avec[currPoint]*(1 + (alphaBeta)*(Tdkm[currPoint]-20)))+
                        pres[currPoint]
  }


 if(length(a$cmscoi) > 0){
   pdkm <- checkOutIndex(a,pdkm)
   pres <- checkOutIndex(a,pres)
   msg <- paste(msg, "skiped points:", a$cmscoi, "see function getOutIndex() why")
 }



  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           "dkm_res",
           calUnit,
           pres)

  ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           "dkm",
           calUnit,
           pdkm,
           msg)

  return(ccc)
  }



sumMpieces <- function(abbList,mNames){
  sumVec <-   rep(0,length(mNames))

  for( i in 1:length(mNames)){
    if(i < 1){
      sumVec[i] <- getConstVal(abbList,mNames[i])
    }else{
      sumVec[i] <- sum(c(sumVec[i-1],getConstVal(abbList,mNames[i])))
    }
  }
  return(sumVec)
}
