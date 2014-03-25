abbrevList <- function(ccc){

  ## die functions checkSetList()
  ## wartet auf Verwendung
  a <- list()
  a$dataAvailable <- FALSE

  a$c        <- ccc$Calibration
  a$cs       <- ccc$Calibration$Standard
  a$ct       <- a$c$Type
  a$cy       <- a$c$Year
  a$csi      <- a$c$Sign
  a$cp       <- a$c$Presettings
  a$cpt      <- a$cp$ToDo
  a$cc       <- a$c$Constants
  a$cm       <- a$c$Measurement
  a$cms      <- a$cm$Standard
  a$cmv      <- a$cm$Values
  a$cma      <- a$cm$AuxValues ## seit 4/11
  a$cmco     <- a$cm$CalibrationObject
  a$cmco1    <- a$cm$CalibrationObject[[1]] ## customer device
  
### hier noch die Co[2...N] explizit trennen

### ce3-spezifisch
  if(a$cs =="CE3"){
<<<<<<< HEAD
   
      a$cmsc   <- a$cm$SequenceControl
      a$cmscok <- a$cmsc$operationKind
      a$cmscg  <- a$cmsc$Gas
      a$cmscp  <- a$cmsc$calPort
=======
    a$cmsc   <- a$cm$SequenzControl
    if(is.null( a$cmsc )){
      a$cmsc <- a$cm$SequenceControl
    }
    a$cmscok <- a$cmsc$operationKind
    a$cmscg  <- a$cmsc$Gas
    a$cmag   <- a$cmscg
    a$cmscp  <- a$cmsc$calPort
>>>>>>> dbddc1e4b228d5429b5a8251b020ce2940cc4964
  }
### se1-spezifisch
  if(a$cs =="SE1"){
      a$cmag <- a$cma$Gas 
  }

### VG spezifisch
  if(a$cs =="FRS5|SE2" |
     a$cs =="DKM|FRS5" |
     a$cs == "FRS5"    |
     a$cs == "DKM"){
      
      if(!(length(a$cm$SequenceControl) == 0)){
          a$cmsc   <- a$cm$SequenceControl
      }
  }
  
  if((is.list(a$cmsc) )){
    a$cmscoi <- a$cmsc$outIndex
  }


  if(length(a$cmv) > 0){
    a$dataAvailable <- TRUE
  }

  a$ca  <- a$c$Analysis
  a$cav <- a$ca$Values
  a$cr  <- a$c$Result

  return(a)
}


