library(R4CouchDB)
print("current folder is:")
print(getwd())

args <- commandArgs(TRUE)
source("lvGetArgs.R")
args <- lvGetArgs(args)


srcPath <- "../utils/"
fn <- list.files(srcPath, pattern=".R$")
for (k in 1:length(fn)) source(paste(srcPath,fn[k],sep=""))

srcPath <- "../cal/"
fn <- list.files(srcPath, pattern=".R$")
for (k in 1:length(fn)) source(paste(srcPath,fn[k],sep=""))


