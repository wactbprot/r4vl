#------------------------------ Funktionen des ce3Mp
srcPath <- "../R/"
fn <- list.files(srcPath, pattern=".R$")
for (k in 1:length(fn)) source(paste(srcPath,fn[k],sep=""))
#------------------------------

##------------------------------ Funktionen des uncert folders
srcPath <- "/home/bock04/eig/ce3/ce3Mp/R4ce3/R4Uncert/"
fn <- list.files(srcPath, pattern=".R$")
for (k in 1:length(fn)) source(paste(srcPath,fn[k],sep=""))
#------------------------------
ccc.cdb <- uncertIni()

ccc <- ccc.cdb$ccc
cdb <- ccc.cdb$cdb

ccc <- uncertPfill(ccc)

## FM3
ccc <- uncertDPfill(ccc)
ccc <- uncertDeltaV(ccc)
ccc <- uncertDeltat(ccc)
ccc <- uncertDeltaVDeltat(ccc)
ccc <- uncertTfm(ccc)
ccc <- uncertFmol(ccc)
ccc <- uncertCmol(ccc)
ccc <- uncertqpV(ccc)
# CE3
ccc <- uncertCx(ccc)
