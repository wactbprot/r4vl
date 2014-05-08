## --
## wactbprot/2011-05-25
## --
library(methods,  quietly =TRUE)
library(bitops,   quietly =TRUE)
library(RJSONIO,  quietly =TRUE)
library(RCurl,    quietly =TRUE)
library(R4CouchDB,quietly =TRUE)

cdb  <- cdbIni()


cdb$DBName     <- "vaclab_db" ## DBName

infList <- list()
infList$srcPath     <-  "/usr/local/lib/r4vl"

setwd(infList$srcPath)

source("load.R")


## devel/tests/calculations from here -----------------
#"1f72ec47286b685511b3cc38f0094533"#"1f72ec47286b685511b3cc38f009ceeb"
# cdb$id     <- "171d3f673527b2564691cad26e0235e7"#
# cdb$id     <- "626b4724c118b38468fc7a15a36e3e52"

cdb$id     <- "7b797897f2572b681bb41c4a7b09766d"
cdb$id     <- "67fd5fc5176edcaa157250a3440ecfb5"
doc <- cdbGetDoc(cdb)$res

if(TRUE){
    
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
#    doc <- ce3.compareCDGs(doc)
#    doc <- writeRes(doc)

}
#cdb$dataList <- doc
#res <- cdbUpdateDoc(cdb)$res
## ----------------------------------------------------

