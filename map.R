#'
#' Function mediates between the callScript and yamp
#' the doc will be updated on the database
#'
#' @author wactbprot (thsteinbock@web.de)
#' @export
#' @keywords yamp
#'

setwd("/usr/local/lib/r4vl")
source("cal.R")
cdb$dataList    <- doc
cat(toJSON(cdbUpdateDoc(cdb)$res))
