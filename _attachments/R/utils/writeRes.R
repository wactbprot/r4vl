writeRes <- function( ccc ){
  msg <- "set by writeRes"

  a <- abbrevList(ccc)

  mbar2Pa       <- getConstVal(a$cc,"mbar_2_Pa")

  ## tested und bereitet rudimentäre Listen vor
  ccc$Calibration$Result        <-  checkSetList(ccc$Calibration$Result)
  ccc$Calibration$Result$Values <-  checkSetList(ccc$Calibration$Result$Values)

  if(length(a$cp) > 0){

    if(length(a$cpt) > 0){
      if((length(a$cpt$Result)) > 0){

        namesRes  <- names(a$cpt$Result)
        noOfNames <- length(namesRes)
        targetUnit <- a$cpt

        for(i in namesRes){ ## das ist z.B. Pressure

          resTypes <- a$cpt$Result[[i]]

          for(j in resTypes){## das ist z.B. ind
            ## msg in for loops immer wieder neu
            ## wenn auch die setCcl in der for loop steht
            msg <- "set by writeRes"

            resToWrite <- getConstVal(a$cav[[i]], j)
            ## autoConversion


            if(resToWrite$Unit == "mbar"){

              resToWrite$Value <- resToWrite$Value * mbar2Pa
              resToWrite$Unit <- "Pa"

              msg <- paste(msg,"converted to Pa with: ", mbar2Pa)
            }


            ccc$Calibration$Result$Values[[i]] <-
              setCcl(ccc$Calibration$Result$Values[[i]] ,
                     resToWrite$Type,
                     resToWrite$Unit,
                     resToWrite$Value,
                     msg)
          }## for resType
        }
      }
    }
  }




return(ccc)

}
