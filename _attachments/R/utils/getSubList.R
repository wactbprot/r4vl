getSubList <- function(ccl, typeName, cc=list()){
  resList <- NULL
  if(is.vector(typeName)){
    for(tn in typeName){
      testList <- gsl(ccl, tn, cc=list())
      if(!is.null(testList)){
        resList <- testList
        break
      }
    }
  }else{
    testList <- gsl(ccl, typeName, cc=list())
  }
  return(resList)
}
## since 13.6.11
gsl <- function(ccl, typeName, cc=list()){

  if(!(is.null(ccl)) & !(is.list(ccl))){

    ## -keine liste
    ## -nicht NULL

    if(ccl[1] == typeName){

      resVal <- cc

      return( resVal )

    }
  }else{
    ## - liste oder
    ## - NUll
    if(is.null(ccl)){return(NULL)}

    for(i in 1:length(ccl)){



      resVal <-   gsl(ccl[[i]], typeName, ccl)

      if(!is.null(resVal)){

        return( resVal )

      }
    }
  }
}
