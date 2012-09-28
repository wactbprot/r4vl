ce3.newCalT <- function(ccc){

  msg <- "calculated by ce3.newCalT"

  a <- abbrevList(ccc)

  N <- length(getConstVal(a$cm,"drift_slope_x"))

  ## T_ptb - Tch als Korrektur benutzt wird, muss
  ## Tch + K gerechnet werden: Tch + K =   Tch + (T_ptb - Tch) = T_ptb

  baseCorrName <- "agilentCorrCh"
  baseNameVal <- "agilentCh"

  chn   <- c("110")
  tRoom <-  getTemperatureVec(a,N,chn,baseNameVal,baseCorrName)

  chn   <- c("103")
  tpbox <-

    chn   <- c("104","105","106","107")
  tUhv <- getTemperatureVec(a,N,chn,baseNameVal,baseCorrName)

  chn   <- c("108","109")

  tXhv <-  getTemperatureVec(a,N,chn,baseNameVal,baseCorrName)

  sufNameVal  <- "_after_lw"
  chn   <- c("101","102")
  tFma <- getTemperatureVec(a,N,chn,baseNameVal,baseCorrName,sufNameVal)

  sufNameVal  <- "_before_lw"
  chn   <- c("101","102")
  tFmb <- getTemperatureVec(a,N,chn,baseNameVal,baseCorrName,sufNameVal)

  tFm <- (tFma+tFmb)/2

  ccc$Calibration$Analysis$Values$Temperature <-
    setCcl( ccc$Calibration$Analysis$Values$Temperature,"Tfm3","K",tFm + C2K,msg)
  ccc$Calibration$Analysis$Values$Temperature <-
    setCcl( ccc$Calibration$Analysis$Values$Temperature,"Tuhv","K",tUhv + C2K,msg)
  ccc$Calibration$Analysis$Values$Temperature <-
    setCcl( ccc$Calibration$Analysis$Values$Temperature,"Txhv","K",tXhv + C2K,msg)
  ccc$Calibration$Analysis$Values$Temperature <-
    setCcl( ccc$Calibration$Analysis$Values$Temperature,"Troom","K",tRoom + C2K,msg)
  ccc$Calibration$Analysis$Values$Temperature <-
    setCcl( ccc$Calibration$Analysis$Values$Temperature,"Tpbox","K",tpbox + C2K,msg)

  return(ccc)

}


