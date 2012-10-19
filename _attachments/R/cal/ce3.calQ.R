ce3.calQ <- function(ccc){
  msg <- "calculated by ce3.calQ"

  a <-  abbrevList(ccc)

  Pam3molK2mbarlmolK <- getConstVal(a$cc, "Pam^3/mol/K_2_mbarl/mol/K")
  R                  <- getConstVal(a$cc,"R") * Pam3molK2mbarlmolK ## in Pa m^3/mol/K

  ## Vorsicht: Cdv ist schon extrapoliert!
  Cdv   <-  getConstVal(a$ca,"cfm3")
  pfill <-  getConstVal(a$ca,"fill")
  TFm   <-  getConstVal(a$ca,"Tfm3")
  TUhv  <-  getConstVal(a$ca,"Tuhv")
  TXhv  <-  getConstVal(a$ca,"Txhv")

  ## für die Eingangsgrößen ist keine outIndex Behandlung mehr nötig,
  ## da alles aus Analysis kommt
  ## also schon über outIndex gelaufen ist

  qmol <- Cdv * pfill /( R * (TFm))

  ## qpVFd gab es bisher nicht ist aber vertretbar
  qpVFd <-  qmol* (TXhv+TUhv)/2 * R

  if(a$cmscok == "opK1"){
    ##    KP1 in Betrieb, XHV-TMP-Ventil offen
    qSplitCorrA <- getConstVal(a$cms,"qSplitCorrUhvOpk1A")
    qSplitCorrB <- getConstVal(a$cms,"qSplitCorrUhvOpk1B")
    qSplitCorrC <- getConstVal(a$cms,"qSplitCorrUhvOpk1C")

    T           <- TUhv 
  }

  if(a$cmscok == "opK2"){
    ##    KP1 und KP2 in Betrieb, UHV und XHV-TMP-Ventil geschlossen
    qSplitCorrA <- getConstVal(a$cms,"qSplitCorrUhvOpk2A")
    qSplitCorrB <- getConstVal(a$cms,"qSplitCorrUhvOpk2B")
    qSplitCorrC <- getConstVal(a$cms,"qSplitCorrUhvOpk2C")

    T           <- TUhv 
  }

  if(a$cmscok == "opK3"){

    ##    KP1 und KP2 in Betrieb, UHV und XHV-TMP-Ventil geschlossen
    ## xhv- Seite
    qSplitCorrA <- getConstVal(a$cms,"qSplitCorrXhvOpk3A")
    qSplitCorrB <- getConstVal(a$cms,"qSplitCorrXhvOpk3B")
    qSplitCorrC <- getConstVal(a$cms,"qSplitCorrXhvOpk3C")

    T           <- TXhv 
  }

  if(a$cmscok == "opK4"){

    ## KP1 in Betrieb, XHV-TMP-Ventil offen
    ## LW wird als linear abh. vom pfill angenommen
    qSplitCorrA <- getConstVal(a$cms,"qSplitCorrUhvOpk1A")
    qSplitCorrB <- getConstVal(a$cms,"qSplitCorrUhvOpk1B")
    qSplitCorrC <- getConstVal(a$cms,"qSplitCorrUhvOpk1C")

    T           <- TUhv 
  }


  fdCorr <-  qSplitCorrA +
    qSplitCorrB * qpVFd +
      qSplitCorrC * qpVFd^2


  qpVCorr <-  qmol * T * R * fdCorr

  ccc$Calibration$Analysis$Values$Flow <-
    setCcl(ccc$Calibration$Analysis$Values$Flow,
           "qmol",
           "mol/s",
           qmol,
           msg
           )

  ccc$Calibration$Analysis$Values$Flow <-
    setCcl(ccc$Calibration$Analysis$Values$Flow,
           "qpV",
           "mbarl/s",
           qpVCorr,
           paste(msg," under ", a$cmscok)
           )

  return (ccc)

}
