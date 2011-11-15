if(length(doc$Calibration) > 0){
  
  doc <- refreshAnalysis(cdb,doc)
  doc <- getOutIndex(doc)
  doc <- ce3.calT(doc)
  doc <- ce3.calPfill(doc)
  doc <- ce3.calDvC(doc)
  doc <- ce3.calQ(doc)
  doc <- ce3.calMfp(doc)
  doc <- ce3.writePind(doc)
  doc <- ce3.calPcal(doc)
  doc <- dispatchResCal(doc)
  
  
  doc <- fm3.uncertPfill(doc)
  doc <- fm3.uncertDPfill(doc)
  doc <- fm3.uncertDeltaV(doc)
  doc <- fm3.uncertDeltaVDeltat(doc)
  doc <- fm3.uncertDeltat(doc)
  doc <- fm3.uncertFmol(doc)
  doc <- fm3.uncertqpV(doc)
  doc <- fm3.uncertCmol(doc)
  
  doc <- ce3.uncertCx(doc)
  doc <- ce3.uncertQsplit(doc)
  doc <- ce3.uncertTfm(doc)
  doc <- ce3.uncertTch(doc)
  doc <- ce3.uncertF(doc)
  doc <- ce3.uncertPcal(doc)
  
}
