if(length(doc$Calibration) > 0){
  
  doc <- refreshAnalysis(cdb,doc)

  doc <- ce3.calDeltaVDeltat(doc)


}
