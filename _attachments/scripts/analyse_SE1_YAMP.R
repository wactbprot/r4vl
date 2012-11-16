if(length(doc$Calibration) > 0){
  ccc <- refreshAnalysis(cdb,ccc)

  
  ccc <- getOutIndex(ccc)
  
  ccc <- se1.calTime(ccc)
  
  ccc <- se1.calT(ccc)
  
  ccc <- se1.calPfill(ccc)
  
  ccc <- se1.calf(ccc)
  ccc <- se1.calRGC(ccc)
  ccc <- se1.calPcal(ccc)

  ccc <- dispatchResCal( ccc )

}
