if(length(doc$Calibration) > 0){

  doc <- refreshResult(cdb,doc)
  doc <- ce3.calTsensKorr(doc)

}
