if(length(doc$Calibration) > 0){

    doc <- refreshAnalysis(cdb,doc)
    doc <- refreshResult(cdb,doc)

    doc <- ce3.newCalT(doc)
    doc <- ce3.newCalPfill(doc)
    doc <- ce3.calDeltaVDeltat(doc)
    doc <- ce3.extrapC(doc)
    doc <- ce3.calQ(doc)
    doc <- ce3.calMfp(doc)
    doc <- ce3.calPcal(doc)
    ##
    doc <- ce3.writePind(doc)
    ##
    doc <- dispatchResCal(doc)

    ## uncertainty ...
    ## ... fm3 related
    doc <- fm3.uncertPfill(doc)
    doc <- fm3.uncertDPfill(doc)
    doc <- fm3.uncertDeltaV(doc)
    doc <- fm3.uncertDeltaVDeltat(doc)
    doc <- fm3.uncertDeltat(doc)

    doc <- fm3.uncertPres(doc)
    doc <- fm3.uncertConstC(doc)

    doc <- fm3.uncertqpV(doc)
    ##  ... ce3 related
    doc <- ce3.uncertCx(doc)
    doc <- ce3.uncertQsplit(doc)
    doc <- ce3.uncertTfm(doc)
    doc <- ce3.uncertTch(doc)
    doc <- ce3.uncertF(doc)
    doc <- ce3.uncertPcal(doc)

    ## ... customer calibration object (cuco) related
    doc <- cuco.uncertDigit(doc)
    doc <- cuco.uncertPOffset(doc)
    doc <- cuco.uncertOffsetDrift(doc)
    doc <- cuco.uncertSync(doc)
    doc <- cuco.uncertExpSd(doc)
    doc <- cuco.uncertGasPurity(doc)
    doc <- cuco.uncertEmis(doc)
    doc <- cuco.uncertPind(doc)
    ## all
    doc <- ce3.uncertTotal(doc)

    ## misc
    doc <- ce3.compareCDGs(doc)
    doc <- writeRes(doc)
}
