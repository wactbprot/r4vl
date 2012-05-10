fm1.uncertqpV <- function(ccc){

  msg <- "Calculated by fm1.uncertqpV()"
  a   <- abbrevList(ccc)

  bord        <- 0.8 ## mbar
  bordUnit    <- "mbar"
  
  PFILL       <-  getSubList(a$cav, "fill")
  pfill       <-  getConstVal(NA,NA,PFILL)
  
  uncertRes   <-  rep(1,length(pfill))
  
  cf          <-  getConvFactor(ccc,bordUnit,PFILL$Unit)

  igfu        <-  which(pfill * cf >=  bord)
  igfl        <-  which(pfill * cf <  bord)
  
  uDPfillList <- getSubList(a$cav, "uncertDPfill")
  uPfillList  <- getSubList(a$cav, "uncertPfill")
  uPresList   <- getSubList(a$cav, "uncertPres")
  uDVList     <- getSubList(a$cav, "uncertDeltaV")
  uDtList     <- getSubList(a$cav, "uncertDeltat")
  
  uCmolList     <- getSubList(a$cav, "uncertCmol")

  if((uPfillList$Unit  ==  uDPfillList$Unit) &
     (uPfillList$Unit  ==  uDVList$Unit)     &
     (uPfillList$Unit  ==  uPresList$Unit)   &
     (uPfillList$Unit  ==  uDtList$Unit)     &
     (uPfillList$Unit  ==  "1")){
    
    if(length(igfu) > 0){
      
      uDPfill <- getConstVal( NA,NA,  uDPfillList)[igfu]
      uPres   <- getConstVal( NA,NA,  uPresList)[igfu]
      uPfill  <- getConstVal( NA,NA,  uPfillList )[igfu]
      uDV     <- getConstVal( NA,NA,  uDVList    )[igfu]
      uDt     <- getConstVal( NA,NA,  uDtList    )[igfu]
   
      
      
      uncertRes[igfu] <- sqrt(uDPfill^2 + uPfill^2 +uPres^2+  uDV^2 + uDt^2)
    }
    if(length(igfl) > 0){

      uDPfill <- getConstVal( NA,NA,  uDPfillList)[igfl]
      uPres   <- getConstVal( NA,NA,  uPresList)[igfl]
      uPfill  <- getConstVal( NA,NA,  uPfillList )[igfl]
      uCmol  <- getConstVal( NA,NA,  uCmolList )[igfl]

      uncertRes[igfl] <- sqrt(uDPfill^2 + uPres^2 + uPfill^2 + uCmol^2)
      
    }
    
  }

  
  ccc$Calibration$Analysis$Values$Uncertainty <-
    setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
           "uncertqpV",
           "1",
           uncertRes,
           msg)
  
  
  return(ccc)
  
}
