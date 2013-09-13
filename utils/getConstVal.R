#'
#' Sucht rekursiv in listen nach
#' Strukturen mit angegebenem Typ
#' 
#' 
getConstVal <- function(subList, type, resList=NULL){

  if(length(resList) == 0){
    if(is.null(resList)){
      resList <-  getSubList(subList,type)
    }
  }

  if(!(length(resList$Value) == 0) ){
      resVal <-  getValVect(resList$Value)
  }else{
      stop(paste("no Value in", type))
  }
  return(resVal)
}
