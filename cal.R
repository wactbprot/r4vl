#'
#' Function mediates between the callScript and yamp
#'
#' @author wactbprot (thsteinbock@web.de)
#' @export
#' @keywords yamp
#'
infList             <- list()
infList$args        <- commandArgs(TRUE) 
infList$callScript  <- infList$args[1]
noOfArgs            <- length(infList$args)

setwd("/usr/local/lib/r4vl")
source("load.R")

cdb             <- cdbIni()
cdb$serverName  <- infList$args[noOfArgs - 2]
cdb$DBName      <- infList$args[noOfArgs - 1]
cdb$id          <- infList$args[noOfArgs]

doc             <- cdbGetDoc(cdb)$res
 
source(infList$callScript)
