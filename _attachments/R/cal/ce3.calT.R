calT <- function(ccc){

  msg <- "calculated by calT"

  a <- abbrevList(ccc)

  C2K <- getConstVal(a$cc, "C_2_K")

  isAgilent <- FALSE
  isPp2     <- FALSE
  ## ist bei der korrektur der NIST Daten eingeführt
  ## muss aber immer solange pp2 benutzt wird so gemacht werden
  corrPp2Slope <- TRUE

  ## etwas plump ...
  if(a$cmv$Temperature[[1]]$Type == "agilentCH01"){
    isAgilent <- TRUE
  }
  if(a$cmv$Temperature[[1]]$Type == "pp2Ch1"){
    isPp2 <- TRUE
  }

  ##
  ## Konsistenprüfungen -> evtl. outlist füllen
  ##

  ## weil T_ptb - Tch als Korrektur benutzt wird, muss
  ## Tch + K gerechnet werden: Tch + K =   Tch + (T_ptb - Tch) = T_ptb

  ## die Task zum auslesen des agilent erzeugt Typen der Form
  ## ---> agilentChxxx mit xxx = 101 ... 110
  if(isAgilent){

    tRoom <-
                (
                 getConstVal(a$cmv,"agilentCHh110") +
                 getConstVal(a$cmco,"agilentCorrCh110")
                 )

      tUhv <- (
               (getConstVal(a$cmv,"agilentCh104") +
                getConstVal(a$cmco,"agilentCorrCh104")) +

               (getConstVal(a$cmv,"agilentCh105") +
                getConstVal(a$cmco,"agilentCorrCh105")) +

               (getConstVal(a$cmv,"agilentCh106") +
                getConstVal(a$cmco,"agilentCorrCh106")) +

               (getConstVal(a$cmv,"agilentCh107") +
                getConstVal(a$cmco,"agilentCorrCh107"))
               ) /4

      tXhv <- (
               (getConstVal(a$cmv,"agilentCh108") +
                getConstVal(a$cmco,"agilentCorrCh108")) +

               (getConstVal(a$cmv,"agilentCh109") +
                getConstVal(a$cmco,"agilentCorrCh109")) +
               ) /2

      tFm <- (
              (getConstVal(a$cmv,"agilentCh101") +
               getConstVal(a$cmco,"agilentCorrCh101")) +

              (getConstVal(a$cmv,"agilentCh102") +
               getConstVal(a$cmco,"agilentCorrCh102"))
               ) /2

    tpbox <-   (getConstVal(a$cmv,"agilentCh103") +
                getConstVal(a$cmco,"agilentCorrCh103"))

    } ## is Agilent


  if(isPp2){


    tch10<- getConstVal(a$cm,"pp2Ch10")

    tch5 <- getConstVal(a$cm,"pp2Ch5")
    tch6 <- getConstVal(a$cm,"pp2Ch6")
    tch7 <- getConstVal(a$cm,"pp2Ch7")
    tch8 <- getConstVal(a$cm,"pp2Ch8")
    tch11<- getConstVal(a$cm,"pp2Ch11")
    tch1 <- getConstVal(a$cm,"pp2Ch1")
    tch1 <- getConstVal(a$cm,"pp2Ch2")

    if(corrPp2Slope){

      ## Rausrechnen der Korrekturen eigentlich
      ## nur für nachträgliche Korrekturen

      Corr.1  <-  getConstVal(a$cmco,"pp2CorrCh1")
      Corr.2  <-  getConstVal(a$cmco,"pp2CorrCh2")
      Corr.4  <-  getConstVal(a$cmco,"pp2CorrCh4")
      Corr.5  <-  getConstVal(a$cmco,"pp2CorrCh5")
      Corr.6  <-  getConstVal(a$cmco,"pp2CorrCh6")
      Corr.7  <-  getConstVal(a$cmco,"pp2CorrCh7")
      Corr.8  <-  getConstVal(a$cmco,"pp2CorrCh8")
      Corr.9  <-  getConstVal(a$cmco,"pp2CorrCh9")
      Corr.10 <-  getConstVal(a$cmco,"pp2CorrCh10")
      Corr.11 <-  getConstVal(a$cmco,"pp2CorrCh11")
      ## - weil rausrechnen
      tch10<- tch10 - Corr.10
      tch5 <- tch5  - Corr.5
      tch6 <- tch6  - Corr.6
      tch7 <- tch7  - Corr.7
      tch8 <- tch8  - Corr.8
      tch11<- tch11 - Corr.11
      tch1 <- tch1  - Corr.1
      tch2 <- tch2  - Corr.2
      msg <- paste(msg, "; const. Korrekturen subtrahiert")

      slopeT10 <- getConstVal(a$cmco,"pp2SlopeCh10")
      intT10   <- getConstVal(a$cmco,"pp2InterceptCh10")
      tch10 <- slopeT10*tch10 + intT10

      slopeT5 <- getConstVal(a$cmco,"pp2SlopeCh5")
      intT5   <- getConstVal(a$cmco,"pp2InterceptCh5")
      tch5 <- slopeT5*tch5 + intT5

      slopeT6 <- getConstVal(a$cmco,"pp2SlopeCh6")
      intT6   <- getConstVal(a$cmco,"pp2InterceptCh6")
      tch6 <- slopeT6*tch6 + intT6

      slopeT7 <- getConstVal(a$cmco,"pp2SlopeCh7")
      intT7   <- getConstVal(a$cmco,"pp2InterceptCh7")
      tch7 <- slopeT7*tch7 + intT7

      slopeT8 <- getConstVal(a$cmco,"pp2SlopeCh8")
      intT8   <- getConstVal(a$cmco,"pp2InterceptCh8")
      tch8 <- slopeT8*tch8 + intT8

      slopeT11 <- getConstVal(a$cmco,"pp2SlopeCh11")
      intT11   <- getConstVal(a$cmco,"pp2InterceptCh11")
      tch11 <- slopeT11*tch11 + intT11

      slopeT1 <- getConstVal(a$cmco,"pp2SlopeCh1")
      intT1   <- getConstVal(a$cmco,"pp2InterceptCh1")
      tch1 <- slopeT1*tch1 + intT1

      slopeT2 <- getConstVal(a$cmco,"pp2SlopeCh2")
      intT2   <- getConstVal(a$cmco,"pp2InterceptCh2")
      tch2 <- slopeT2*tch2 + intT2

      msg <- paste(msg, "; Temperaturen mit slope und Intercept korrigiert")
    }

    tRoom    <-  tch10
    tUhv     <-  (tch5 + tch6 + tch7 ) / 3
    tXhv     <-  (tch8 + tch11 ) / 2
    tFm      <-  tch1
    tpbox    <-  tch2

  } ## isPp2

  if(length(a$cmscoi) > 0){
    if(a$cmscoi[1] > 0){

      tRoom <- tRoom[-a$cmscoi]
      tUhv<-tUhv[-a$cmscoi]
      tXhv<-tXhv[-a$cmscoi]
      tFm<-tFm[-a$cmscoi]
      tpbox<-tpbox[-a$cmscoi]

    }
  }

  ## gibt
  ## es Analysis_Values
  ## schon?:
  ccc$Calibration$Analysis$Values <- checkSetList(ccc$Calibration$Analysis$Values)


  ccc$Calibration$Analysis$Values$Temperature <-
    setCcl( ccc$Calibration$Analysis$Values$Temperature,"Tfm3","K",tFm + C2K,msg)
  ccc$Calibration$Analysis$Values$Temperature <-
    setCcl( ccc$Calibration$Analysis$Values$Temperature,"Tuhv","K",tUhv + C2K,msg)
  ccc$Calibration$Analysis$Values$Temperature <-
    setCcl( ccc$Calibration$Analysis$Values$Temperature,"Txhv","K",tXhv + C2K)
  ccc$Calibration$Analysis$Values$Temperature <-
    setCcl( ccc$Calibration$Analysis$Values$Temperature,"Troom","K",tRoom + C2K)
  ccc$Calibration$Analysis$Values$Temperature <-
    setCcl( ccc$Calibration$Analysis$Values$Temperature,"Tpbox","K",tpbox + C2K)

  return(ccc)

}
