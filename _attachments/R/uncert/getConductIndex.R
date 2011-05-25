getConductIndex <- function(ccc){
  ## hier können verschiedene Verfahren der
  ## Ermittlung der jeweils zuständigen
  ## Leitwerte implementiert werden
  ## return Val bezieht sich

  tmpAn     <- ccc$Calibration$Analysis
  tmpMea    <- ccc$Calibration$Measurement
  tmpStrd   <- tmpMea$Standard
  seqCtrl   <- tmpMea$SequenzConrol

### Verf. 1: Lw Type in seqCtrl vorh.
  if(length(seqCtrl$usedConductance) > 0){
    iLw1 <- which(seqCtrl$usedConductance == "Lw1")
    iLw2 <- which(seqCtrl$usedConductance == "Lw2")
    ## entsprechende pfill extrahieren
  }

### Verf. 2: Lw Type wird aus Conduc. nachträglich bestimmt
  ## Lw2
  lw2List <- getSubList(tmpStrd, "useLw2")

  if(lw2List$RangeUnit == "l/s"){
    cfm3List <- getSubList(tmpAn, "cfm3")
    if(cfm3List$Unit == lw2List$RangeUnit){
      iLw2 <- which((cfm3List$Value > as.double(lw2List$From)) &
                    (cfm3List$Value < as.double(lw2List$To)))
    }else{
      print("getConductIndex: Units (useLW1$RangeUnit and cfm3$Unit) dont match")
      stop()
    }
  }

  ## Lw1
  lw1List <- getSubList(tmpStrd, "useLw1")

  if(lw1List$RangeUnit == "l/s"){
    cfm3List <- getSubList(tmpAn, "cfm3")
    if(cfm3List$Unit == lw1List$RangeUnit){
      iLw1 <- which((cfm3List$Value > as.double(lw1List$From)) &
                    (cfm3List$Value < as.double(lw1List$To)))
    }else{
      print("getConductIndex: Units (useLW2$RangeUnit and cfm3$Unit) dont match")
      stop()
    }
  }

  if(length(iLw1) + length(iLw2) == length(cfm3List$Value)){
    ##
    return(list(iLw1=iLw1,   ## grosser LW
                iLw2=iLw2))  ## kleiner LW

  }else{
    print("index do not cover entire value range")
    stop()
  }
}
