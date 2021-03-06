fm3.uncertPfill <- function(ccc){

  msg <- "Calculated by fm3.uncertPfill()"
  a   <- abbrevList(ccc)
 
  PFILL     <- getSubList(a$cav, "fill")
  pfill     <- getConstVal(NA,NA,PFILL)

  noOfCo    <- length( a$cmco)
  noOfPfill <- length(PFILL$Value)

  uncertPfillRes <- rep(NA,noOfPfill)

  if( noOfCo < 2){
    print("No. of CalibrationObjects < 2")
    stop()
  }else{

    for(ico in 1:noOfCo){
      currCo <- a$cmco[[ico]]
      ## Vorsicht es werden alle Calibration Objekte angesehen!!
      ## deshalb noch UsedFor Tag eingef.
      ## ok!
      ## function abstrahiert mit quadrSumContrib()
      ## die Tatsache, dass man die function calVarianz()
      ## aufrufen kann und nur pfill bezogene Anteile berücksichtigt,
      ## liegt an der if Abfrage  des usedFor Tags
      if(length(currCo$Device$UsedFor) > 0){
        if(currCo$Device$UsedFor == "pfill"){
          ## der uncertPfillRes param muß übergeben werden, weil
          ## unterschiedliche CalibrationObjects unterschiedliche Bereich
          ## des  pfill ausfüllen
          res <- quadrSumContrib(currCo, PFILL,uncertPfillRes,msg)
          uncertPfillRes <- res$uncertRes
          msg <- res$msg
        }
      }
    }
    
    ccc$Calibration$Analysis$Values$Uncertainty <-
      setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
             "uncertPfill",
             "1",
             uncertPfillRes,
             msg[length(msg)])

  }## noOfCo > 2
  return(ccc)
}
