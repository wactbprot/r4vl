getConstVal <- function(subList,type, resList=NULL){

  if(length(resList) == 0){
    if(is.null(resList)){
      resList <-  getSubList(subList,type)
    }
  }

  if(!(length(resList$Value) == 0) ){
    ## der eigentliche benefit
    ## verarbeitet auch folgendes Bsp.:
    ## > as.double(c(1.3,3,"4"))
    ## [1] 1.3 3.0 4.0

    resVal <-  lapply(resList$Value ,
                      function(i){
                        if(is.null(i)){
                          return(NA)
                        }else{
                          return(as.double(i))
                        }
                      })

    resVal <- as.vector(unlist(resVal))

  }else{

    stop(paste("no Value in", type))
  }
  return(resVal)
}
