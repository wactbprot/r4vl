ce3.calDvC <- function( ccc){

  msg <- "calculated by calDvC()"

  a <- abbrevList(ccc)

  ## später muss der Leitwert aus dem Analysis Block kommen
  ## !! die TypeNames müssen bisher noch eindeutig sein
  if(a$cmscok == "opK1" | a$cmscok == "opK2" | a$cmscok == "opK3") {

    Ldv <- getConstVal(a$cm,"cfm3")

    if(length(a$cmscoi) > 0){
      if(a$cmscoi[1] > 0){

        Ldv <- Ldv[-a$cmscoi]
      }
    }
  } ## opk1-3

  ## const. LW method
  if(a$cmscok == "opK4"){

    pfillList <- getSubList(a$ca,"fill")

    if(pfillList$Unit == "mbar"){

      pfill <- getConstVal(NA, "fill", pfillList)

      dv2MolCIntercept <- getConstVal(a$cms, "dv2MolCIntercept")
      dv2MolCSlope     <- getConstVal(a$cms, "dv2MolCSlope")

      Ldv <- dv2MolCSlope * pfill + dv2MolCIntercept

      msg <- paste(msg, " with ", dv2MolCSlope, " * pfill + ", dv2MolCIntercept)

    }else{
      stop()
    }
  } ## opk4

  msg <- paste(msg, "using case", a$cmscok)

  ccc$Calibration$Analysis$Values$Conductance <-
    setCcl(ccc$Calibration$Analysis$Values$Conductance,"cfm3","l/s",Ldv, msg)

  return(ccc)

}
