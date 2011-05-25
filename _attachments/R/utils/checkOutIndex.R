checkOutIndex <- function(a, checkVec){
  
  if((length(a$cmscoi) > 0) & (a$cmscoi[1] > 0)){

    clearVec <- checkVec[-a$cmscoi]
    
  }else{
    clearVec <- checkVec
  }
  

  return(clearVec)
}
