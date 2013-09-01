library(R4CouchDB,
        quietly = TRUE)


cdb <- cdbIni()

cdb$id     <- "726f33302c032c22be609c94b80318c9"
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

if(length(ccc$Calibration) > 0){

  ccc <- refreshAnalysis(cdb,ccc)
  ccc <- getOutIndex(ccc)


  ccc <- frs5.calPfrs5(ccc)
  ccc <- frs5.uncertPfrs5(ccc)
  ccc <- dispatchResCal(ccc)

  ##  cdb$dataList <- ccc

  ##  res <- cdbAddDoc(cdb)$res
}
