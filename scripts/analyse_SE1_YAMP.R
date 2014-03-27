if(length(doc$Calibration) > 0){
    doc <- se1.calTime(doc)
    doc <- se1.calT(doc)
    doc <- se1.calPfill(doc)
    doc <- se1.calf(doc)
    doc <- se1.calRGC(doc)
    doc <- se1.calPcal(doc)
    
    doc <- se1.uncertPfill(doc)
    doc <- se1.uncertf(doc)
    doc <- se1.uncertdT(doc)
    doc <- se1.uncertT1(doc)
    doc <- se1.uncertRg(doc)
    doc <- se1.uncertAds(doc)
    doc <- se1.uncertVz(doc)
    doc <- se1.uncertGas(doc)
    doc <- se1.uncertAtm(doc)
    doc <- se1.uncertValve(doc)
    doc <- se1.uncertInh(doc)
    doc <- se1.uncertPres(doc)
    doc <- se1.uncertRep(doc)

    ## ... customer calibration object (cuco) related

    doc <- se1.uncertTotal(doc)    
}
