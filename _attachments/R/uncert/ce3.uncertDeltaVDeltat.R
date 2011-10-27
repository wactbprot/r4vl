ce3.uncertDeltaVDeltat <- function(ccc){
  msg <- "Calculated by ce3.uncertDeltaVDeltat()"

  
  a <- abbrevList(ccc)
  
  PFILL     <- getSubList(a$cav, "fill")
  pfill     <- getConstVal(NA,NA,PFILL)
  pfillUnit <- PFILL$Unit
    

  uncertRes <- rep(1,length(pfill))

  iLw1      <-  getConductIndex(ccc)$iLw1
  iLw2      <-  getConductIndex(ccc)$iLw2

  if(length(iLw1) > 0){

    u1List         <- getSubList(a$cms,"fm3DeltaVDeltatLw1_u1")
    iu1            <- checkUncertRange(u1List, PFILL, iLw1)
    uncertRes[iu1] <- getConstVal(NA,NA,u1List)
    
    msg <- paste(msg,
                 "points: ",
                 toString(iu1),
                 "belong to Lw1, use: ",
                 u1List$Type)
  }

  if(length(iLw2) > 0){

    u1aList <- getSubList(a$cms,"fm3DeltaVDeltatLw2_u1_a")
    iu1a    <- checkUncertRange(u1aList, PFILL, iLw2)

    u1bList <- getSubList(a$cms,"fm3DeltaVDeltatLw2_u1_b")
    iu1b    <- checkUncertRange(u1bList, PFILL, iLw2)

    u1cList <- getSubList(a$cms,"fm3DeltaVDeltatLw2_u1_c")
    iu1c    <- checkUncertRange(u1cList, PFILL, iLw2)

    ## es geht hier (nachfolg. if statement darum die derzeitige impl.
    ## der beiden Unsicherh. sicherzustellen
    if(length(iu1a) == length(iu1b)){

      uncertRes[iu1b] <-
        getConstVal(NA,NA,u1aList) + log(1/pfill[iu1b]) * getConstVal(NA,NA, u1bList)
      
      msg <- paste(msg,
                   "points: ",
                   toString(iu1b),
                   "belong to Lw2, use: ",
                   u1aList$Type,
                   " and ",
                   u1bList$Type)
    }

    if(length(iu1c) > 0){
      uncertRes[iu1c] <- getConstVal(NA,NA,u1cList)

      msg <- paste(msg,
                   "points: ",
                   toString(iu1c),
                   "belong to Lw2, use: ",
                   u1cList$Type)
    }
  }

  ccc$Calibration$Analysis$Values$Uncertainty <-
    setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
           "uncertDeltaVDeltat",
           "1",
           uncertRes,
           msg)
  
  
  return(ccc)
}
