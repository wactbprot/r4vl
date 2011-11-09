if(length(doc$Calibration) > 0){

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
