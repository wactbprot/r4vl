getTemperatureVec <- function(a,N,channelNames,baseTName,baseCorrName, sufVal="",sufCorr=""){

  noOfCh <- length(channelNames)
  resMat <- matrix(ncol=N, nrow=noOfCh)
  
  for(i in 1:noOfCh){
      
      tName    <- paste(baseTName,    channelNames[i],sufVal, sep="")
      corrName <- paste(baseCorrName, channelNames[i],sufCorr, sep="")
      resMat[i,] <-  getConstVal(a$cmv,tName ) + 
        getConstVal(a$cmco,corrName) 
  }
  return( colMeans(resMat))
}
