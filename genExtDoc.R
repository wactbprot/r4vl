## --
## wactbprot/2013-09-13
## --

test  <- TRUE
extdb <- "vaclab_ext"
if(!test){
    infList             <- list()
    infList$args        <- commandArgs(TRUE) 
    noOfArgs            <- length(infList$args)
}

tmpPath                 <- "/tmp/"

if(!test){
    instPath            <- "/usr/local/lib/r4vl/"
    setwd(instPath)
    source("load.R")
    cdb                 <- cdbIni()
    cdb$serverName      <- infList$args[noOfArgs - 2]
    cdb$DBName          <- infList$args[noOfArgs - 1]
    cdb$id              <- infList$args[noOfArgs]
}else{
    source("load.R")
    cwd                 <- getwd()
    cdb                 <- cdbIni()
    cdb$DBName          <- "vaclab_db" 
#    cdb$id              <- "f481e565fd252673c6a6e7b6b8003f05" #CE3
    cdb$id              <- "4a279906e8f7855fea31e7a18c044774" #SE1
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
ndoc           <- list()
ndoc[["_id"]]  <- cdb$id
ndoc$Standard  <- a$cs
ndoc$Sign      <- a$csi
ndoc$Type      <- a$ct
ndoc$Year      <- a$cy

outdb          <- cdbIni() 
outdb$DBName   <- extdb
outdb$id       <- cdb$id
outdb$dataList <- ndoc

cat(toJSON(cdbUpdateDoc(outdb)$res))
