#'
#' Function mediates between the callScript and yamp
#'
#' @author wactbprot (thsteinbock@web.de)
#' @export
#' @keywords yamp
#'

infList             <- list()
infList$args        <- commandArgs(TRUE) 
## load map env 
noOfArgs            <- length(infList$args)
setwd("/usr/local/lib/r4vl")

infList$callScript  <- infList$args[1]

source("load.R")

source("load.R")

cdb             <- cdbIni()
cdb$serverName  <- infList$args[noOfArgs - 2]
cdb$DBName      <- infList$args[noOfArgs - 1]
cdb$id          <- infList$args[noOfArgs]

## try:
doc             <- cdbGetDoc(cdb)$res

source(infList$callScript)
