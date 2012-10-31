## --
## wactbprot/2012-10-31
## --

infList             <- list()
infList$args        <- commandArgs(TRUE) 
## load map env 
noOfArgs            <- length(infList$args)
infList$srcPath     <- infList$args[1]
infList$callScript  <- infList$args[2]

setwd(infList$srcPath)

source("load.R")

cdb             <- cdbIni()
cdb$serverName  <- infList$args[noOfArgs - 2]
cdb$DBName      <- infList$args[noOfArgs - 1]
cdb$id          <- infList$args[noOfArgs]

## try:
doc             <- cdbGetDoc(cdb)$res

source(infList$callScript)
