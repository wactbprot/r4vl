if(length(doc$Calibration) > 0){



    doc <- se1.calTime(doc)
    doc <- se1.calT(doc)
    doc <- se1.calPfill(doc)
    doc <- se1.calf(doc)
    doc <- se1.calRGC(doc)
    doc <- se1.calPcal(doc)
    doc <- se1.writePind(doc)

    doc <- dispatchResCal(doc)

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

    a   <- abbrevList( doc )
    
   if(a$cs == "SE1" & a$cpt$Type == "srg_error"){
       doc <- cuco.uncertVisc(doc)
       doc <- cuco.uncertDigit(doc)
       doc <- cuco.uncertPOffset(doc)
       doc <- cuco.uncertOffsetDrift(doc)
       doc <- cuco.uncertExpSd(doc)
       doc <- cuco.uncertPrise(doc)
   }

#    doc <- se1.uncertPcal(doc)
#    doc <- cuco.uncertPind(doc)
#    
#    doc <- se1.uncertTotal(doc)
#    ## misc
#    doc <- writeRes(doc)
}
