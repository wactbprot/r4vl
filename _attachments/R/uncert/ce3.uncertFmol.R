ce3.uncertFmol <- function(ccc){
  
  msg <- "Calculated by ce3.uncertFmol()"
  a   <- abbrevList(ccc)
  
  
  
  PFILL  <- getSubList(a$cav, "fill")
  pfill  <- getConstVal(NA,NA,PFILL)
  
  uncertRes <- rep(0.0, length(pfill))

  iLw1 <-  getConductIndex(ccc)$iLw1
  iLw2 <-  getConductIndex(ccc)$iLw2

  ## Lw1:
  if(length(iLw1) > 0){

    u1aList <-  getSubList(a$cms,"fm3FmolLw1_u1_a")
    iu1a    <- checkUncertRange(u1aList, PFILL, iLw1)

    u1bList <-  getSubList(a$cms,"fm3FmolLw1_u1_b")
    iu1b    <- checkUncertRange(u1bList, PFILL, iLw1)

    if((length(iu1b) == length(iu1a) ) & (length(iu1a) > 0)){

      uncertRes[iu1b] <- getConstVal(NA,NA,u1aList) + getConstVal(NA,NA,u1bList) * pfill[iu1b]

      msg <- paste(msg,
                   "points: ",
                   toString(iu1b),
                   " use: ",
                   u1aList$Type,
                   "(LW1) and ",
                   u1bList$Type,
                   "(LW1)")
    }

    u2List <-  getSubList(a$cms,"fm3FmolLw1_u2")
    iu2    <-  checkUncertRange(u2List, PFILL, iLw1)
    
    if(length(iu2) > 0 ){
      
      uncertRes[iu2] <- getConstVal(NA,NA,u2List)

      msg <- paste(msg,
                   "points: ",
                   toString(iu2),
                   " use: ",
                   u2List$Type,
                   "(LW1)")
    }

  }## LW1

  ## Lw1:
  if(length(iLw2) > 0){

    u1aList <-  getSubList(a$cms,"fm3FmolLw2_u1_a")
    iu1a    <-  checkUncertRange(u1aList, PFILL, iLw2)

    u1bList <-  getSubList(a$cms,"fm3FmolLw2_u1_b")
    iu1b    <-  checkUncertRange(u1bList, PFILL, iLw2)

    if((length(iu1b) == length(iu1a) )& length(iu1a) > 0){

      uncertRes[iu1b] <- getConstVal(NA,NA,u1aList) + getConstVal(NA,NA,u1bList) * pfill[iu1b]

      msg <- paste(msg,
                   "points: ",
                   toString(iu1b),
                   " use: ",
                   u1aList$Type,
                   " (LW2) and ",
                   u1bList$Type,
                   "(LW2)")
    }
    
    u2List <-  getSubList(a$cms,"fm3FmolLw2_u2")
    iu2    <-  checkUncertRange(u2List, PFILL, iLw1)

    if(length(iu2) > 0 ){
      uncertRes[iu2] <- getConstVal(NA,NA,u2List)
      msg <- paste(msg,
                   " points: ",
                   toString(iu2),
                   " use: ",
                   u2List$Type,
                   " (LW2)")
    }

  }## LW2
  ccc$Calibration$Analysis$Values$Uncertainty <-
    setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
           "uncertFmol",
           "1",
           uncertRes,
           msg)
  
  return(ccc)
  }


##      REAL FUNCTION S11(p0,ILeit) !F_mol (Kap.2.1)
##      S11=0.
## c    if(p0.lt.10) S11=1.69e-6+(5.e-3-5.e-4*p0)**2   !Gl. (35)

##      if(p0.lt.10.and.Ileit.eq.1) S11=1.3e-2-1.3e-3*p0       !Publ.
### [1.3e-3] = 1/Pa --> 0.13 1/mbar
##      if(p0.lt.1.and.Ileit.eq.1) S11=1.2e-2                   !Publ.

##      if(p0.lt.10.and.Ileit.eq.2) S11=1.6e-2-1.6e-3*p0       !Publ.
### [1.6e-3] = 1/Pa --> 0.16 1/mbar
##      if(p0.lt.1.and.Ileit.eq.2) S11=1.5e-2                   !Publ.
##      S11=S11*S11
##      END
