getTemperatureVec <- function(a,N,channelNames,baseTName,baseCorrName){

  noOfCh <- length(channelNames)
  
  resMat <- matrix(ncol=N, nrow=noOfCh)
  
  for(i in 1:noOfCh){
    
    tName    <- paste(baseTName,    channelNames[i], sep="")
    corrName <- paste(baseCorrName, channelNames[i], sep="") 
    
    resMat[i,] <-  getConstVal(a$cmv,tName ) + 
      getConstVal(a$cmco,corrName) 
    
  }
  
return( colMeans(resMat))
}
