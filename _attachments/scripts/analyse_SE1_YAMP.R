if(length(doc$Calibration) > 0){
  doc <- refreshAnalysis(cdb,doc)
  doc <- refreshResult(cdb,doc)   
  
  doc <- se1.calTime(doc)
  doc <- se1.yamp.calT(doc)

  doc <- se1.calPfill(doc)

  doc <- se1.yamp.calf(doc)
  doc <- se1.calRGC(doc)
  doc <- se1.calPcal(doc)
  
  doc <- dispatchResCal( doc )
  
}
