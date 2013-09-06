## --
## wactbprot/2012-08-14
## --
library(xlsx,     quietly =TRUE)
library(knitr,    quietly =TRUE)
library(reshape2, quietly =TRUE)
library(ggplot2,  quietly =TRUE)
library(methods,  quietly =TRUE)
library(bitops,   quietly =TRUE)
library(RJSONIO,  quietly =TRUE)
library(RCurl,    quietly =TRUE)
library(R4CouchDB,quietly =TRUE)

utilsPath     <- "./utils/"
calPath       <- "./cal/"
uncertPath    <- "./uncert/"
scriptPath    <- "./scripts/"
srcPat        <- "R$"

fn <- list.files(utilsPath, pattern=srcPat)
for (k in 1:length(fn)){
  source(paste(utilsPath,fn[k],sep=""))
}

fn <- list.files(calPath, pattern=srcPat)
for (k in 1:length(fn)){
  source(paste(calPath,fn[k],sep=""))
}

fn <- list.files(uncertPath, pattern=srcPat)
for (k in 1:length(fn)){
  source(paste(uncertPath,fn[k],sep=""))
}
## 

