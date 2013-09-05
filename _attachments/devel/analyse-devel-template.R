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
infList$srcPath     <-  "/usr/local/src/map/_attachments/"

setwd(infList$srcPath)

source("load.R")


## devel/tests/calculations from here -----------------
cdb$id         <- "7fae64fa9f90ad02320c90696a0097dd"
doc <- cdbGetDoc(cdb)$res

 if(length(doc$Calibration) > 0){

    doc <- ce3.compareCDGs(doc)
 ## > doc$Calibration$Analysis$AuxValues
 ## $Pressure
 ## $Pressure$Type
 ## [1] "dpfill"
 ## 
 ## $Pressure$Unit
 ## [1] "1"
 ## 
 ## $Pressure$Value
 ## [1] -0.02279198166037 -0.00681538071554  0.12771535580525 -0.00705722202039
 ## 
 ## $Pressure$Comment
 ## [1] "calculated by ce3.compareCDGs ; dpfill =  (pcdga  - p0cdga)/(pcdgb  - p0cdgb) -1"



}




## ----------------------------------------------------
Sys.setenv("no_proxy" = saveNoProxy)
