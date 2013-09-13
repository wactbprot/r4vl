#'
#' Funktion wandelt
#' `liste()` von Werten in vector
#' 
getValVect <- function(valList){
    ## der eigentliche Benefit
    ## verarbeitet auch folgendes Bsp.:
    ## > as.double(c(1.3,3,"4"))
    ## [1] 1.3 3.0 4.0
    return(as.vector(unlist(lapply(valList,
                                   function(num){
                                       if(is.null(num)){
                                           return(NA)
                                       }else{
                                           return(as.double(num))
                                       }
                                   }
                                   )
                            )
                     )
           )
}
