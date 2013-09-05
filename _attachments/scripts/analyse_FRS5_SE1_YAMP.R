if(length(doc$Calibration) > 0){

  doc <- refreshAnalysis(cdb,doc)
  doc <- se1.yamp.calPfill(doc)
  doc <- se1.calRGC(doc)
  doc <- frs5_se1.yamp.calT(doc)
  doc <- frs5.calPfrs5(doc)
  doc <- calVFrs5Se1ExpA(doc)
  
  #doc <- frs5.uncertPfrs5(doc)

}
