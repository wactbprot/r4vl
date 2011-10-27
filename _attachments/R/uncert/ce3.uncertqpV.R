ce3.uncertqpV <- function(ccc){

  msg <- "Calculated by ce3.uncertqpV()"
  a   <- abbrevList(ccc)
  
  uPfillList <- getSubList(a$cav, "uncertPfill")
  uDVList    <- getSubList(a$cav, "uncertDeltaV")
  uDtList    <- getSubList(a$cav, "uncertDeltat")
  uDVDtList  <- getSubList(a$cav, "uncertDeltaVDeltat")
### ist das alles????
  if((uPfillList$Unit  ==  uDVList$Unit)    &
     (uPfillList$Unit  ==  uDVDtList$Unit)  &
     (uPfillList$Unit  ==  uDtList$Unit)    &
     (uPfillList$Unit  ==  "1")){

    uncertRes <- sqrt(uPfillList$Value^2 +  uDVList$Value^2 + uDtList$Value^2 + uDVDtList$Value^2)

    ccc$Calibration$Analysis$Values$Uncertainty <-
      setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
             "uncertqpV",
             "1",
             uncertRes,
             msg)
  }
  
  return(ccc)

}
