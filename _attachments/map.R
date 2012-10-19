## --
## wactbprot/2012-08-14
## --
library(methods,  quietly =TRUE)
library(bitops,   quietly =TRUE)
library(RJSONIO,  quietly =TRUE)
library(RCurl,    quietly =TRUE)
library(R4CouchDB,quietly =TRUE) 

infList             <- list()

infList$args        <- commandArgs(TRUE) 

## load map env 
infList$srcPath     <- infList$args[1]
infList$callScript  <- infList$args[2]

setwd(infList$srcPath)
infList$utilsPath     <- "./R/utils/"
infList$calPath       <- "./R/cal/"
infList$uncertPath    <- "./R/uncert/"
infList$scriptPath    <- "./scripts/"
infList$srcPat        <- "R$"

fn <- list.files(infList$utilsPath, pattern=infList$srcPat)
for (k in 1:length(fn)){
  source(paste(infList$utilsPath,fn[k],sep=""))
}

fn <- list.files(infList$calPath, pattern=infList$srcPat)
for (k in 1:length(fn)){
  source(paste(infList$calPath,fn[k],sep=""))
}

fn <- list.files(infList$uncertPath, pattern=infList$srcPat)
for (k in 1:length(fn)){
  source(paste(infList$uncertPath,fn[k],sep=""))
}
## 

cdb             <- cdbIni()
cdb$serverName  <- infList$args[3]
cdb$DBName      <- infList$args[4]
cdb$id          <- infList$args[5]

## try:
doc             <- cdbGetDoc(cdb)$res

source(infList$callScript)

cdb$dataList    <- doc

cat(toJSON(cdbUpdateDoc(cdb)$res))
