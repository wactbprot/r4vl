## --
## wactbprot/2011-05-25
## --
library(methods,  quietly =TRUE)
library(bitops,   quietly =TRUE)
library(RJSONIO,  quietly =TRUE)
library(RCurl,    quietly =TRUE)
library(R4CouchDB,quietly =TRUE)

cdb  <- cdbIni()


cdb$DBName     <- "uml_test" ## DBName

infList <- list()
infList$srcPath     <-  "/usr/local/src/map/_attachments/"

setwd(infList$srcPath)

source("load.R")


## devel/tests/calculations from here -----------------
#"1f72ec47286b685511b3cc38f0094533"#"1f72ec47286b685511b3cc38f009ceeb"

cdb$id     <- "5c9f2659545fe56374723402d2038d50"

doc <- cdbGetDoc(cdb)$res

doc <- refreshAnalysis(cdb,doc)

doc <- se1.calTime(doc)
doc <- se1.calT(doc)
doc <- se1.calPfill(doc)
doc <- se1.calf(doc)
doc <- se1.calRGC(doc)
doc <- se1.calPcal(doc)

doc <- dispatchResCal( doc )
                                        #cdb$dataList <- doc
                                        #cdbUpdateDoc(cdb)
## ----------------------------------------------------
Sys.setenv("no_proxy" = saveNoProxy)
