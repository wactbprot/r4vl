library(R4CouchDB,
        quietly = TRUE)


cdb <- cdbIni()

cdb$id     <- "7f696c493207f8d813f3b4e1f05355c2"
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
ccc <- se2.calAddVolume(ccc)

ccc <- calfFrs5Se2(ccc)

a <- abbrevList(ccc)
f.alt <- 9.1854e-3
f.neu <- getConstVal(a$cav, "f_pure")

f.neu/f.alt -1


##  cdb$dataList <- ccc
##  res <- cdbAddDoc(cdb)$res

