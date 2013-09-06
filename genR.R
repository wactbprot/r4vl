## --
## wactbprot/2013-09-05
## --

infList             <- list()
infList$args        <- commandArgs(TRUE) 
noOfArgs            <- length(infList$args)

instPath            <- "/usr/local/lib/r4vl/"
templatePath        <- "templates/"
figurePath          <- "figures/"
tmpPath             <- "/tmp/"

setwd(instPath)
 
infList$callScript  <- infList$args[1]

source("load.R")
 
cdb                 <- cdbIni()
cdb$serverName      <- infList$args[noOfArgs - 2]
cdb$DBName          <- infList$args[noOfArgs - 1]
cdb$id              <- infList$args[noOfArgs]
## 
## 
## 
doc                 <- cdbGetDoc(cdb)$res
a                   <- abbrevList(doc)
reportName          <- paste(a$cy,a$ct,a$cs,a$csi, sep="-")
outPath             <- paste(tmpPath, reportName,"/", sep="")

dir.create(outPath,
           showWarnings = FALSE,
           mode = "0777")
setwd(outPath)
dir.create(paste(outPath,
                 figurePath , sep=""),
           showWarnings = FALSE,
           mode = "0777")

 ## generate report
opts_chunk$set(fig.width=8, fig.height=6, fig.path=figurePath)
options( digits = 8)

if(a$cs == "CE3"){
    knit(paste(instPath, templatePath, "ce3.report.Rhtml", sep=""),
         paste(reportName, ".html", sep=""))
}

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

figures <- list.files(figurePath, pattern="png$")
 
for( fig in figures){
    outdb$fileName <-  paste(figurePath, fig, sep="")
    tmp            <- cdbAddAttachment(outdb)$res
}

outdb$fileName     <-  paste(reportName, ".html", sep="")
tmp                <- cdbAddAttachment(outdb)$res

## excel-land:

xlsxName <- paste(reportName, ".xlsx", sep="")

if(is.data.frame(df.conductance)){

    write.xlsx(df.conductance, 
           sheetName = "Conductance", 
           xlsxName)

}

if(is.data.frame(df.temperature)){
    write.xlsx(df.temperature, 
               sheetName = "Temperature", 
               xlsxName,
               append=TRUE)
    
}


outdb$fileName     <- xlsxName
tmp                <- cdbAddAttachment(outdb)$res
