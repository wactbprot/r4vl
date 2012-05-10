fm1.uncertDeltaV <- function(ccc){

  msg <- "Calculated by fm1.uncertDeltaV()"

  
  a <- abbrevList(ccc)

  
  PFILL     <- getSubList(a$ca, "fill")
  pfill     <- getConstVal(NA,NA,PFILL)
  pfillUnit <- PFILL$Unit

  DIST      <- getSubList(a$ca, "l")
  dist      <- getConstVal(NA,NA,DIST)
  distUnit  <- DIST$Unit

  noOfPfill <- length(pfill)
  ## Ergebnissvektor mit 1 initialisieren
  uncertDeltaV <- rep(1,noOfPfill)
  ## bezugsmenge Wasser
  deltaGList <-  getSubList(a$cms,"deltaG")
  deltaG <- getConstVal(NA,NA,deltaGList)

  
  u1aList <-  getSubList(a$cms,"fm1DeltaV_u1_a")
  u1aAbs <- getConstVal(NA,NA,u1aList)

  u1cList <-  getSubList(a$cms,"fm1DeltaV_u1_c")
  u1cAbs <- getConstVal(NA,NA,u1cList)

  if(u1aList$Unit ==  deltaGList$Unit){
    u1a <-  u1aAbs/deltaG
  }else{
    stop("unit deltaG and fm1DeltaV_u1_a don't match")
  }
  if(u1cList$Unit ==  deltaGList$Unit){
    u1c <-  u1cAbs/deltaG
  }else{
    stop("unit deltaG and fm1DeltaV_u1_c don't match")
  }

  
  u1bList <- getSubList(a$cms,"fm1DeltaV_u1_b")
  u1b <-  getConstVal(NA,NA,u1bList)
  iu1b <- checkUncertRange(u1bList, PFILL)

  
  u1dList <- getSubList(a$cms,"fm1DeltaV_u1_d")
  u1d <-  getConstVal(NA,NA,u1dList)
  iu1d <- checkUncertRange(u1dList, PFILL)

  ## fm3DeltaV_u1_e wird auf ges. Verfahrweg bezogen
  u1eList <- getSubList(a$cms,"fm1DeltaV_u1_e")
  u1eAbs <-  getConstVal(NA,NA,u1eList)
  
  iu1e <- checkUncertRange(u1eList, PFILL)

  if(u1eList$Unit ==  DIST$Unit){
    u1e <-  u1eAbs/dist
  }else{
    stop("unit distance and fm1DeltaV_u1_e don't match")
  }
  
  u1fList <- getSubList(a$cms,"fm1DeltaV_u1_f")
  u1f <-  getConstVal(NA,NA,u1fList)
  iu1f <- checkUncertRange(u1fList, PFILL)

  u1gList <- getSubList(a$cms,"fm1DeltaV_u1_g")
  u1g <-  getConstVal(NA,NA,u1gList)
  iu1g <- checkUncertRange(u1gList, PFILL)

  u1hList <- getSubList(a$cms,"fm1DeltaV_u1_h")
  u1h <-  getConstVal(NA,NA,u1hList)
  iu1h <- checkUncertRange(u1hList, PFILL)
  
  if((length(iu1d) == length(iu1e)) &
     (length(iu1e) == length(iu1f)) &
     (length(iu1f) == length(iu1g)) &
     (length(iu1g) == length(iu1h))){

       uncertDeltaV[iu1h] <- sqrt(u1a^2+
                                  u1c^2+
                                  u1b^2+
                                  u1d^2+
                                  u1e^2+
                                  u1f^2+
                                  u1g^2+
                                  u1h^2)

       
       ccc$Calibration$Analysis$Values$Uncertainty <-
         setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
                "uncertDeltaV",
                "1",
                uncertDeltaV,
                msg)
     }else{
       stop("at least one DeltaV uncertainty contrib. has a wrong length")
     }
     return(ccc)
   }
  
