if(length(doc$Calibration) > 0){
  doc <- refreshAnalysis(cdb,doc)
  doc <- refreshResult(cdb,doc)   
  doc <- getOutIndex(doc)
  doc <- se1.calTime(doc)
  doc <- se1.calT(doc)
 doc <- se1.calPfill(doc)
 doc <- se1.calf(doc)
 doc <- se1.calRGC(doc)
 doc <- se1.calPcal(doc)

 doc <- dispatchResCal( doc )

}
