getSubList <- function(ccl, typeName, cc=list()){

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



      resVal <-   getSubList(ccl[[i]], typeName, ccl)

      if(!is.null(resVal)){

        return( resVal )

      }
    }
  }
}
