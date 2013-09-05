if(length(doc$Calibration) > 0){

  doc <- refreshAnalysis(cdb,doc)
  doc <- getOutIndex(doc)


  doc <- frs5.calPfrs5(doc)
  doc <- frs5.uncertPfrs5(doc)
  doc <- dispatchResCal(doc)
}
