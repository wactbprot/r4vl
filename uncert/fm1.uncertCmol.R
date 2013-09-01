fm1.uncertCmol <- function(ccc){
  
  msg <- "Calculated by fm1.uncerCmol()"
  a   <- abbrevList(ccc)
  
  PFILL  <- getSubList(a$cav, "fill")
  pfill  <- getConstVal(NA,NA,PFILL)

  ## punkte, die nicht in diesen bereich fallen,
  ## haben diese Unsicherheit nicht
  ## d.h. res vektor kann mit 0en initialisiert werden
  ures <- rep(NA,length(pfill))
  
  U1A       <-  getSubList(a$cms,"fm1Cmol_u1_a")
  U1B       <-  getSubList(a$cms,"fm1Cmol_u1_b")
  i1a        <- checkUncertRange(U1A, PFILL)
  
  if((length(i1a) > 0) & (!(i1a[1] == 0))){
    k            <- getConvFactor(ccc,U1A, PFILL)
    ures[i1a]    <- getConstVal(NA,NA,U1A)/(pfill[i1a]*k) +
      getConstVal(NA,NA,U1B) 
  }

  U2A       <-  getSubList(a$cms,"fm1Cmol_u2_a")
  U2B       <-  getSubList(a$cms,"fm1Cmol_u2_b")
  i2a        <- checkUncertRange(U2A, PFILL)
  
  if((length(i2a) > 0) & (!(i2a[1] == 0))){
    k            <- getConvFactor(ccc,U2A, PFILL)
    ures[i2a]    <- getConstVal(NA,NA,U2A)/(pfill[i2a]*k) +
      getConstVal(NA,NA,U2B) 
  }

  U3A       <-  getSubList(a$cms,"fm1Cmol_u3_a")
  U3B       <-  getSubList(a$cms,"fm1Cmol_u3_b")
  
  i3a        <- checkUncertRange(U3A, PFILL)

  
  if((length(i3a) > 0) & (!(i3a[1] == 0))){
    k            <- getConvFactor(ccc,U3A, PFILL)
    ures[i3a]    <- getConstVal(NA,NA,U3A)/(pfill[i3a]*k) +
      getConstVal(NA,NA,U3B) 
  }
 
  ccc$Calibration$Analysis$Values$Uncertainty <-
    setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
           "uncertCmol",
           "1",
           ures,
           msg)
  
  return(ccc)
}
