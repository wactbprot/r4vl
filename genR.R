## --
## wactbprot/2013-09-05
## --

test                <- TRUE
tmpPath             <- "/tmp/"
extdb               <- "vaclab_ext"
templatePath        <- "templates/"
figurePath          <- "figures/"

if(!test){
    infList         <- list()
    infList$args    <- commandArgs(TRUE)
    noOfArgs        <- length(infList$args)

    instPath        <- "/usr/local/lib/r4vl/"
    setwd(instPath)
    source("load.R")
    cdb             <- cdbIni()
    cdb$serverName  <- infList$args[noOfArgs - 2]
    cdb$DBName      <- infList$args[noOfArgs - 1]
    cdb$id          <- infList$args[noOfArgs]

    outdb           <- cdbIni()
    outdb$DBName    <- extdb
    outdb$id        <- cdb$id

}else{
    source("load.R")
    cwd             <- getwd()
    instPath        <- cwd     
    cdb             <- cdbIni()
    cdb$DBName      <- "vaclab_db"
    cdb$id          <- "f481e565fd252673c6a6e7b6b8003f05"
}

doc                 <- cdbGetDoc(cdb)$res
a                   <- abbrevList(doc)
reportName          <- paste(a$cy,a$ct,a$cs,a$csi, sep="-")
outPath             <- paste(tmpPath, reportName,"/", sep="")

dir.create(outPath,
           showWarnings = FALSE,
           mode = "0777")

dir.create(paste(outPath,
                 figurePath , sep=""),
           showWarnings = FALSE,
           mode = "0777")

## ---- generate report
setwd(outPath)
opts_chunk$set(fig.width=8, fig.height=6, fig.path=figurePath)
options( digits = 8)

if(a$cs == "CE3"){
    knit(paste(instPath,"/",templatePath, "ce3.report.Rhtml", sep=""),
         paste(reportName, ".html", sep=""))
}

## ---- upload report and figures
outdb          <- cdbIni()
outdb$DBName   <- extdb
outdb$id       <- cdb$id

figures        <- list.files(figurePath, pattern="png$")
for( fig in figures){
    outdb$fileName <-  paste(figurePath, fig, sep="")
    tmp            <- cdbAddAttachment(outdb)$res
}
outdb$fileName     <-  paste(reportName, ".html", sep="")
tmp                <- cdbAddAttachment(outdb)$res

setwd(cwd)
