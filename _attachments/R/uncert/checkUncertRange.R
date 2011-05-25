checkUncertRange <- function(uncertIList, referenceList, globalRange=NA){

  referenceVal     <- referenceList$Value
  ## gri: global range index
  ## lri: lokal range index

  ## globalRange ok------------------------------
  if(length(names(globalRange)) > 0){
    if((length(globalRange$From) > 0) &
       (length(globalRange$To) > 0)   &
       (length(globalRange$RangeUnit) > 0)){

      if(globalRange$RangeUnit == referenceList$Unit){
        globalFrom <- as.double(globalRange$From)
        globalTo <- as.double(globalRange$To)

        gri <- which((referenceVal >= globalFrom)  &
                     (referenceVal <= globalTo))
      }else{
        print("global Range Unit dont match")
        stop()
      }
    }
  }

  if(length(names(globalRange)) == 0){
    if(length(globalRange) == 1){
      if(is.na(globalRange)){ ## wenn z.B

        globalFrom <- min(referenceVal)
        globalTo <- max(referenceVal)
        globalRangeUnit <- referenceList$Unit

        gri <- which((referenceVal >= globalFrom)  &
                     (referenceVal <= globalTo))
      }

      if(is.integer(globalRange)){
        if(globalRange > 0){
          gri <- globalRange
        }
      }
    }

    if(length(globalRange) > 1){
      if(is.integer(globalRange[1])){
        if(globalRange[1] > 0){
          gri <- globalRange
        }
      }
    }
  }

  ## local range------------------------------
  if((length(uncertIList$From) > 0) &
     (length(uncertIList$To) > 0)   &
     (length(uncertIList$RangeUnit) > 0)){

    if((uncertIList$RangeUnit == referenceList$Unit)){
      ## die vorliegende Unsicherheit hat eine range
      uncertIFrom <-  as.double(uncertIList$From)
      uncertITo   <-  as.double(uncertIList$To)

      lri  <- which((referenceVal > uncertIFrom) &
                    (referenceVal < uncertITo)   )


    }else{
      msg <- paste(msg,("uncertRangeUnit don't match!"))
      stop()
    }
  }else{
    ## die vorliegende Unsicherheit hat keine range
    lri <- gri
  }

  return(gri[gri %in% lri])
}
