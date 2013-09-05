## --
## wactbprot/2012-10-31
## --

infList             <- list()
infList$args        <- commandArgs(TRUE) 
## load map env 
noOfArgs            <- length(infList$args)
setwd("/usr/local/lib/r4vl")

infList$callScript  <- infList$args[1]

source("load.R")

cdb             <- cdbIni()
cdb$serverName  <- infList$args[noOfArgs - 2]
cdb$DBName      <- infList$args[noOfArgs - 1]
cdb$id          <- infList$args[noOfArgs]
## 
## ## try:

doc             <- cdbGetDoc(cdb)$res
 
source(infList$callScript)
 
cdb$dataList    <- doc
cat(toJSON(cdbUpdateDoc(cdb)$res))
