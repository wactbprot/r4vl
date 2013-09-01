##library(R4CouchDB)

srcPath <- "~/se1Repos/R4CouchDB/R4CouchDB/R/"
fn <- list.files(srcPath, pattern=".R$")
print("kkkkkkkkkkkk")
for (k in 1:length(fn)) source(paste(srcPath,fn[k],sep=""))


args <- commandArgs(TRUE)


## ---------------- wird später durch:
## library(analyseSe1)
## ---------------- ersetzt

srcPath <- "../utils/"
fn <- list.files(srcPath, pattern=".R$")

for (k in 1:length(fn)) source(paste(srcPath,fn[k],sep=""))
srcPath <- "../cal/"

fn <- list.files(srcPath, pattern="^se1\\..*\\.R$")
for (k in 1:length(fn)) source(paste(srcPath,fn[k],sep=""))
## ----------------

args <- getArgs(args)

cdb <- cdbIni()

cdb$DBName     <- args$DBName
cdb$serverName <- args$host
cdb$id         <- args$id

ccc <- cdbGetDoc(cdb)$res
ccc <- refreshAnalysis(cdb,ccc)   

ccc <- getOutIndex(ccc)

ccc <- se1.calTime(ccc)

ccc <- se1.calT(ccc)

ccc <- se1.calPfill(ccc)

ccc <- se1.calf(ccc)
ccc <- se1.calRGC(ccc)
ccc <- se1.calPcal(ccc)

ccc <- dispatchResCal( ccc )

a   <- abbrevList( ccc )

if(a$dataAvailable){
  
  cdb$dataList <- ccc

  print(ccc)
  cdbUpdateDoc(cdb)
  
}else{
  
  print(paste("keine daten in", cdb$baseAdr(cdb)))
  
}
