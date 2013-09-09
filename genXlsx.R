## --
## wactbprot/2013-09-05
## --
test <- TRUE
if(!test){
    infList             <- list()
    infList$args        <- commandArgs(TRUE) 
    noOfArgs            <- length(infList$args)
}

instPath            <- "/usr/local/lib/r4vl/"
tmpPath             <- "/tmp/"

setwd(instPath)

source("load.R")

cdb                     <- cdbIni()
if(!test){
    cdb$serverName      <- infList$args[noOfArgs - 2]
    cdb$DBName          <- infList$args[noOfArgs - 1]
    cdb$id              <- infList$args[noOfArgs]
}else{
    cdb$DBName          <- "vaclab_db" 
    cdb$id              <- "910fd907311a80c24cee9ad18a07bf58"
}

doc                 <- cdbGetDoc(cdb)$res
a                   <- abbrevList(doc)

reportName          <- paste(a$cy,a$ct,a$cs,a$csi, sep="-")
outPath             <- paste(tmpPath, reportName,"/", sep="")

dir.create(outPath,
           showWarnings = FALSE,
           mode = "0777")
setwd(outPath)

## build new doc
ndoc <- list()
ndoc[["_id"]] <- cdb$id
ndoc$Standard <- a$cs
ndoc$Sign     <-a$csi
ndoc$Type     <-a$ct
ndoc$Year     <-a$cy

outdb         <- cdbIni() 
outdb$DBName  <- "vaclab_ext"
outdb$id      <- cdb$id
outdb$dataList <- ndoc
try(cdbUpdateDoc(outdb)$res, TRUE)

## -----------excel-land:

xlsxName <- paste(reportName, ".xlsx", sep="")

lnames   <- names(valList)
for(sheetName in lnames){

    df <- makeDf(valList[[sheetName]])
    write.xlsx()
}



if(!test){
    outdb$fileName     <- xlsxName
    tmp                <- cdbAddAttachment(outdb)$res
}
