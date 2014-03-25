if(length(doc$Calibration) > 0){
  doc <- refreshAnalysis(cdb,doc)
  doc <- refreshResult(cdb,doc)   
  
  doc <- se1.calTime(doc)
  doc <- se1.yamp.calT(doc)

  doc <- se1.calPfill(doc)

  doc <- se1.calf(doc)
  doc <- se1.calRGC(doc)
  doc <- se1.calPcal(doc)

  doc <- dispatchResCal( doc )

  
  doc <- se1.uncertPfill(doc)
  doc <- se1.uncertf(doc)
}
