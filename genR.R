

basePath <- "/home/bock04/eig/r4vl/"
templatePath <- "templates/"
reportPath <- "reports/"
figurePath <- "figure/"
setwd(basePath)

source("load.R")

gl <- cdbIni() ## getList
pl <- cdbIni() ##putList

gl$DBName <- "vaclab_db"
gl$id     <- "910fd907311a80c24cee9ad18a07d57b"

pl$DBName <- "vaclab_blob"
pl$id     <- gl$id



doc <- cdbGetDoc(gl)$res
a   <- abbrevList(doc)

reportName <- paste(a$cy,a$ct,a$cs,a$csi, sep="-")

if(a$cs == "CE3"){
    knit(paste(basePath,templatePath, "ce3.report.Rhtml", sep=""),
         paste(basePath,reportPath,reportName, ".html", sep=""))
     }

## --build new doc--

ndoc <- list()
ndoc[["_id"]] <- gl$id
ndoc$Standard <- a$cs
ndoc$Sign     <-a$csi
ndoc$Type     <-a$ct
ndoc$Year     <-a$cy

pl$dataList <- ndoc
try(cdbUpdateDoc(pl)$res, TRUE)


figures <- list.files(paste(basePath, figurePath, sep=""),
                      pattern="png$")

for( fig in figures){
pl$fileName <-  paste(figurePath, fig, sep="")
cdbAddAttachment(pl)$res
}

pl$fileName <-  paste(basePath,reportPath,reportName, ".html", sep="")
cdbAddAttachment(pl)$res
