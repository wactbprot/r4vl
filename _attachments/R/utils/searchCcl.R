searchCcl <- function(ccl, typeName, cc=list()){

  if(!is.null(ccl) & !is.list(ccl)){

    if(ccl[1] == typeName){

      resVal <- cc
      return( resVal )

    }

  }else{

    for(i in 1:length(ccl)){

      resVal <-   searchCcl(ccl[[i]], typeName, ccl)

      if(!is.null(resVal)){

        return( resVal )

      }
    }
  }
}
