library(R4CouchDB,
        quietly = TRUE)


cdb <- cdbIni()

cdb$id     <- "7f696c493207f8d813f3b4e1f037207b"
cdb$DBName <- "vaclab_db"

## ----------------
srcPath <- "../utils/"
fn <- list.files(srcPath, pattern=".R$")
for (k in 1:length(fn)) source(paste(srcPath,fn[k],sep=""))

srcPath <- "../cal/"
fn <- list.files(srcPath, pattern=".R$")
for (k in 1:length(fn)) source(paste(srcPath,fn[k],sep=""))

srcPath <- "../uncert/"
fn <- list.files(srcPath, pattern=".R$")
for (k in 1:length(fn)) source(paste(srcPath,fn[k],sep=""))
## ----------------

ccc <- cdbGetDoc(cdb)$res

ccc <- refreshAnalysis(cdb,ccc)

ccc <- frs5_se2.yamp.calT(ccc)

ccc <- frs5.calPfrs5(ccc)
ccc <- frs5.uncertPfrs5(ccc)
ccc <- se2.calPfill(ccc)
ccc <- se2.calRGC(ccc)
ccc <- calfFrs5Se2(ccc)
ccc <- se2.calAddVolume(ccc)
##  cdb$dataList <- ccc
##  res <- cdbAddDoc(cdb)$res

