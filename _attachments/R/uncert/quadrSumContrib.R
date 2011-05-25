quadrSumContrib <- function( currentCalObj, refQuantityList,uncertRes, msg ){

  noOfRefQuantity <- length(refQuantityList$Value)

  currCORangeList <- getSubList(currentCalObj,"useDev")
    ## die range des CalibrationObjekts muss definiert sein:

  if(length(currCORangeList) > 0){
    currentCalObjUncert <- currentCalObj$Uncertainty
    noOfUncertContrib <- length(currentCalObjUncert)
    if(noOfUncertContrib > 0){

      varianzMat <- matrix(ncol=noOfUncertContrib, nrow=noOfRefQuantity, NA)

      for(iUncert in 1:noOfUncertContrib){
        uncertIList <- currentCalObjUncert[[iUncert]]
        idx<- checkUncertRange(uncertIList, refQuantityList, currCORangeList)
        if(length(idx) > 0){
          ##------------------------------
          ## die eigentlichen Rechnungen
          ## neue Units hier impl.
          ##------------------------------
          if(uncertIList$Unit =="1"){
            ## Unsicherheit ist schon relativ
            msg <- paste(msg,"use",uncertIList$Type,"for point(s)", toString(idx))
            varianzI <- as.double(uncertIList$Value)^2
          }## unit==1
          ##------------------------------
          if(uncertIList$Unit =="mbar"){
            ## (u_i/pfill)^2
            varianzI <- (as.double(uncertIList$Value)/refQuantityList$Value[idx])^2
            msg <- paste(msg,"use",uncertIList$Type,
                         "/ ", refQuantityList$Type, " in",uncertIList$Unit, "for point(s)", toString(idx))
          }## unit==mbar
          ##------------------------------
          if(uncertIList$Unit =="K"){

            ## (u_i/pfill)^2
            varianzI <- (as.double(uncertIList$Value)/refQuantityList$Value[idx])^2
            msg <- paste(msg,"use",uncertIList$Type,
                         "/ ", refQuantityList$Type," in ",uncertIList$Unit, "for point(s)", toString(idx))
          }## unit==mbar
          ##------------------------------------------------------------

          varianzMat[idx,iUncert] <- varianzI

        }## mehr als 0 ipfill
      }## next u_i

      ifullCORange <- checkUncertRange(list(), refQuantityList , currCORangeList)
      ## quadratisches aufsumieren allen Beiträge:

      uncertRes[ifullCORange] <- sqrt(rowSums(varianzMat[ifullCORange, ],  na.rm=TRUE))

    }else{
      print(paste( currentCalObj$Device$Name, "do not have a uncertainty definitions"))
    }
  }else{## co hat undefinierte Range
    print(paste( currentCalObj$Device$Name , "do not have a valid Range definition"))
  }

  return(res <- list(uncertRes=uncertRes, msg=msg))
}
