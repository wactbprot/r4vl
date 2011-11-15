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

}
