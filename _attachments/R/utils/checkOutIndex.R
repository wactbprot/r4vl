checkOutIndex <- function(a, checkVec){

  clearVec <- checkVec

  if(length(a$cmscoi) > 0){
    if(a$cmscoi[1] > 0){

      clearVec <- checkVec[-a$cmscoi]

    }
  }


  return(clearVec)
}
