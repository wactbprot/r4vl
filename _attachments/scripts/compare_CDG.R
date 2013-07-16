if(length(doc$Calibration) > 0){
  doc <- ce3.compareCDGs(doc)

  deviation <- getConstVal(doc$Calibration$Analysis$AuxValues, "dpfill")
  
  cat(toJSON(list("deviation"   = deviation )))


}
