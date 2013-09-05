## run with rproc
if(length(doc$Calibration) > 0){

  doc <- refreshAnalysis(cdb,doc)
  doc <- getOutIndex(doc)

  doc <- dkm.calPdkm(doc)
  doc <- frs5.calPfrs5(doc)

  doc <- dkm.uncertPdkm(doc)
  doc <- frs5.uncertPfrs5(doc)

  doc <- calEn(doc)


}
