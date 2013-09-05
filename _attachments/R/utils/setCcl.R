setCcl <- function(where,Type,Unit,Value,Comment=""){

  options(digits=12)
  aimStruct <- where

  ## das item existiert noch gar nicht:
  if(is.null(aimStruct)){

    aimStruct$Type    <- Type
    aimStruct$Unit    <- Unit
    aimStruct$Value   <- Value
    aimStruct$Comment <-  Comment

    ##          print("new")
  }else{
    if(is.list(aimStruct[[1]])){
      ## es existieren bereits einige Einträge
      ## es muss noch auf Type getestet werden !!
      lastEntry <- length(aimStruct)
      pos <- lastEntry +1

      for(k in 1:lastEntry){
        if(aimStruct[[k]]$Type == Type){
          pos <- k
        }
      }
      aimStruct[[pos ]] <- list()

      aimStruct[[pos]]$Type    <- Type
      aimStruct[[pos]]$Unit    <- Unit
      aimStruct[[pos]]$Value   <- Value
      aimStruct[[pos]]$Comment <- Comment
      ##     print("struct")

    }else{## Path existiert bereits mit nur einem Eintrag (also nur ein Type vorhanden)
      if(aimStruct$Type == Type){ ## Eintrag ist der selbe wie angefordert

        aimStruct$Type    <- Type
        aimStruct$Unit    <- Unit
        aimStruct$Value   <- Value
        aimStruct$Comment <-  Comment
        ## print("replaced")
      }else{ # es existiert bereits _ein_ anderer Eintrag

        aimStructType   <-  aimStruct$Type
        aimStructUnit   <-  aimStruct$Unit
        aimStructValue  <-  aimStruct$Value
        aimStructComment<-  aimStruct$Comment

        aimStruct       <- list()
        aimStruct[[1]]  <- list()
        aimStruct[[2]]  <- list()

        aimStruct[[1]]$Type    <- if(is.null(aimStructType)){""}else{   aimStructType   }
        aimStruct[[1]]$Unit    <- if(is.null(aimStructUnit)){""}else{   aimStructUnit   }
        aimStruct[[1]]$Value   <- if(is.null(aimStructValue)){""}else{  aimStructValue  }
        aimStruct[[1]]$Comment <- if(is.null(aimStructComment)){""}else{aimStructComment}



        aimStruct[[2]]$Type    <- Type
        aimStruct[[2]]$Unit    <- Unit
        aimStruct[[2]]$Value   <- Value
        aimStruct[[2]]$Comment <- Comment


        ## print("shift")
      }
    }

  }
  where <- aimStruct
  return(where)
}

