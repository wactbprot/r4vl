giveLastPos <- function(SignVec, Sign){

  eqCount <- which(SignVec == Sign)

  lenEqCount <- length(eqCount)
  if(lenEqCount >0){
    takePos <- eqCount[lenEqCount]
  }else{
    takePos <- 0
  }
  return(takePos)
}
