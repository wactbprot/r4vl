getConductIndex <- function(ccc){

  ## hier können verschiedene Verfahren der
  ## Ermittlung der jeweils zuständigen
  ## Leitwerte implementiert werden
  ## return Val bezieht sich

  a <- abbrevList(ccc)
  res <- list()
  ## Verf. 1: Lw Type in a$cmsc vorh.
  if(length(a$cmsc$usedConductance) > 0){

    iLw1     <- which(a$cmsc$usedConductance == "Lw1")
    iLw2     <- which(a$cmsc$usedConductance == "Lw2")
    ## entsprechende pfill extrahieren

    res$iLw1 <- iLw1   ## grosser LW
    res$iLw2 <- iLw2    ## kleiner LW
    
  }

  ## Verf. 2: Lw Type wird aus Conduc. nachträglich bestimmt
  CFM3       <- getSubList(a$cav, "cfm3")

  if(length(CFM3$Unit) == 0){
    CFM3       <- getSubList(a$cav, "cnom")
  }


  
  lwUnit     <- "l/s"
  ## --- Lw2
  lw2List    <- getSubList(a$cms, "useLw2")


  if(length(CFM3$Unit) == 1 & CFM3$Unit == lwUnit){
    if(lw2List$RangeUnit ==  lwUnit){
      if(CFM3$Unit == lw2List$RangeUnit){
        iLw2 <- which((getConstVal(NA,NA,CFM3) > as.double(lw2List$From)) &
                      (getConstVal(NA,NA,CFM3) < as.double(lw2List$To)))
        
      }else{
        print("getConductIndex: Units (useLW1$RangeUnit and cfm3$Unit) dont match")
        stop()
      }
    }
    ## --- Lw1
    lw1List <- getSubList(a$cms, "useLw1")
    if(lw1List$RangeUnit ==  lwUnit){
      if(CFM3$Unit       == lw1List$RangeUnit){
        iLw1 <- which((getConstVal(NA,NA,CFM3) > as.double(lw1List$From)) &
                      (getConstVal(NA,NA,CFM3) < as.double(lw1List$To)))
      }else{
        print("getConductIndex: Units (useLW2$RangeUnit and cfm3$Unit) dont match")
        stop()
      }
    }
    
    if(length(iLw1) + length(iLw2) == length(getConstVal(NA,NA,CFM3))){
      

    res$iLw1 <- iLw1   ## grosser LW
    res$iLw2 <- iLw2    ## kleiner LW
      
    }else{
      print("index do not cover entire value range")
      stop()
    }
  }

  if(length(res$iLw1) >0 || length(res$iLw2) >0  ){
    return(res)
  }
}

