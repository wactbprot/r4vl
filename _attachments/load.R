## --
## wactbprot/2012-08-14
## --
library(methods,  quietly =TRUE)
library(bitops,   quietly =TRUE)
library(RJSONIO,  quietly =TRUE)
library(RCurl,    quietly =TRUE)
library(R4CouchDB,quietly =TRUE) 


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

