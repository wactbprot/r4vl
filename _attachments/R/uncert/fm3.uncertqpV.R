fm3.uncertqpV <- function(ccc){

  msg <- "Calculated by fm3.uncertqpV()"
  a   <- abbrevList(ccc)

  bord        <- 0.01 ## mbar
  bordUnit    <- "mbar"
  
  PFILL       <-  getSubList(a$cav, "fill")
  pfill       <-  getConstVal(NA,NA,PFILL)
  
  uncertRes   <-  rep(0,length(pfill))
  
  cf          <-  getConvFactor(ccc,bordUnit,PFILL$Unit)

  igfu        <-  which(pfill * cf >=  bord)
  igfl        <-  which(pfill * cf <  bord)
  
  uDPfillList <- getSubList(a$cav, "uncertPfill")
  uPfillList  <- getSubList(a$cav, "uncertPfill")
  uDVList     <- getSubList(a$cav, "uncertDeltaV")
  uDtList     <- getSubList(a$cav, "uncertDeltat")
  uDVDtList   <- getSubList(a$cav, "uncertDeltaVDeltat")
  #uPresList  <- getSubList(a$cav, "")
  #uPresCmol  <- getSubList(a$cav, "")
  
  if((uPfillList$Unit  ==  uDPfillList$Unit) &
     (uPfillList$Unit  ==  uDVList$Unit)     &
     (uPfillList$Unit  ==  uDVDtList$Unit)   &
     (uPfillList$Unit  ==  uDtList$Unit)     &
     (uPfillList$Unit  ==  "1")){
    
    if(length(igfu) > 0){
      
      uDPfill <- getConstVal( NA,NA,  uDPfillList)[igfu]
      uPfill  <- getConstVal( NA,NA,  uPfillList )[igfu]
      uDV     <- getConstVal( NA,NA,  uDVList    )[igfu]
      uDt     <- getConstVal( NA,NA,  uDtList    )[igfu]
      uDVDt   <- getConstVal( NA,NA,  uDVDtList  )[igfu]
      
      
      uncertRes[igfu] <- sqrt(uDPfill^2 + uPfill^2 +  uDV^2 + uDt^2 + uDVDt^2)
    }
    if(length(igfl) > 0){
     ## wenn XHV-KP wieder läuft
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
