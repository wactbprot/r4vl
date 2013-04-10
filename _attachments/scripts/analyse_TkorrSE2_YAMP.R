if(length(doc$Calibration) > 0){

  doc <- refreshResult(cdb,doc)
  doc <- se2.calTsensKorr(doc)

}
