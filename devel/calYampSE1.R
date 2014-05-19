## --
## wactbprot/2014-03-25
## --

infList             <- list()
infList$srcPath     <-  "/usr/local/lib/r4vl"
setwd(infList$srcPath)

source("load.R")

cdb            <- cdbIni()
cdb$DBName     <- "vaclab_work" ## DBName


## devel/tests/calculations from here -----------------
                                        #"1f72ec47286b685511b3cc38f0094533"
                                        #"1f72ec47286b685511b3cc38f009ceeb"
                                        # "5c9f2659545fe56374723402d2038d50"
#cdb$id     <- "2d54e68feffe46b5fa0ecf788a0bc0f2" # ITRC SRG 2013
cdb$id     <- "f5de71877189703b76447b49f833b551" # ITRC SRG 2014
doc <- cdbGetDoc(cdb)$res

doc <- refreshAnalysis(cdb,doc)
doc <- refreshResult(cdb,doc)
 
a   <- abbrevList(doc)

doc <- se1.calTime(doc)
doc <- se1.calT(doc)
doc <- se1.calPrise(doc)
doc <- se1.calPfill(doc)
doc <- se1.calf(doc)
doc <- se1.calRGC(doc)
doc <- se1.calPcal(doc)

doc <- se1.writePind(doc)

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

if(a$cs == "SE1" & a$cpt$Type == "srg_error"){
    doc <- cuco.uncertVisc(doc)
    doc <- cuco.uncertDigit(doc)
    doc <- cuco.uncertPOffset(doc)
    doc <- cuco.uncertOffsetDrift(doc)
    doc <- cuco.uncertExpSd(doc)
    doc <- cuco.uncertPrise(doc)
}

doc <- se1.uncertPcal(doc)
doc <- cuco.uncertPind(doc)

doc <- se1.uncertTotal(doc)

doc <- dispatchResCal(doc)
doc <- dispatchResSum(doc)

###cdb$dataList <- doc
###cdbUpdateDoc(cdb)

