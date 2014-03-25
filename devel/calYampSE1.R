## --
## wactbprot/2014-03-25
## --

infList             <- list()
infList$srcPath     <-  "/usr/local/lib/r4vl"
setwd(infList$srcPath)

source("load.R")

cdb            <- cdbIni()
cdb$DBName     <- "vaclab_test" ## DBName


## devel/tests/calculations from here -----------------
                                        #"1f72ec47286b685511b3cc38f0094533"
                                        #"1f72ec47286b685511b3cc38f009ceeb"
                                        # "5c9f2659545fe56374723402d2038d50"
cdb$id     <- "2d54e68feffe46b5fa0ecf788a0bc0f2" # ITRC SRG 2013
doc <- cdbGetDoc(cdb)$res

doc <- refreshAnalysis(cdb,doc)

doc <- se1.calTime(doc)
doc <- se1.calT(doc)
doc <- se1.calPfill(doc)
doc <- se1.calf(doc)
doc <- se1.calRGC(doc)
doc <- se1.calPcal(doc)

doc <- se1.uncertPfill(doc)
doc <- se1.uncertf(doc)

doc <- dispatchResCal( doc )


###cdb$dataList <- doc
###cdbUpdateDoc(cdb)

