## --
## wactbprot/2011-05-25
## --
library(methods,  quietly =TRUE)
library(bitops,   quietly =TRUE)
library(RJSONIO,  quietly =TRUE)
library(RCurl,    quietly =TRUE)
library(R4CouchDB,quietly =TRUE)

cdb  <- cdbIni()
loadSrc <- TRUE
cdb$DBName     <- "vaclab_db" ## DBName

saveNoProxy <- Sys.getenv("no_proxy")
Sys.setenv("no_proxy" = "*")


if(loadSrc){

  cdb$id <- paste("_design/map",sep="")
  srcDoc <- cdbGetDoc(cdb)$res
  files <- names(srcDoc$'_attachments')

  baseSrcUrl <- paste(cdb$baseUrl(cdb),
                      cdb$DBName,"/",
                      cdb$id,"/",
                      sep="")

  for(file in  files){
    fn <- grep("^R/.*\\.R$",file)

    if(length(fn) > 0){
        srcUrl <- paste(baseSrcUrl,
                        file,
                        sep="")

        source(srcUrl)

      }
  }
}

## edit --------------------
gas <- "Ar"

xlab  <- "p in mbar"
ylab  <- "C in l/s"

lwName <- "klLw"
baseName <- paste(lwName,gas, sep="")
main  <- baseName
pdfname <- paste(baseName,".pdf", sep="")
datname <-  paste(baseName,".dat", sep="")
## -------------------------

setwd("/home/bock04/eig/map/_attachments/datasets/")

data <- read.table(datname, sep=" ", dec=".")
pp <- data[,2]
cp <- data[,1]


cf <- list()
cf$a  <-  getConstVal(a$cms, paste(lwName,"_",gas,"_A", sep=""))
cf$b  <-  getConstVal(a$cms, paste(lwName,"_",gas,"_B", sep=""))
cf$c  <-  getConstVal(a$cms, paste(lwName,"_",gas,"_C", sep=""))
cf$d  <-  getConstVal(a$cms, paste(lwName,"_",gas,"_D", sep=""))

pg <- seq(min(pp),max(pp), min(pp))

##setwd("/home/bock04/eig/map/_attachments/diag")
pdf(pdfname)
plot(pp,cp, log="x", xlab=xlab, ylab=ylab, main=main)
points(pg,fn.2162(cf,pg), type="l", col=2)
       grid(col=1)
##dev.off()
       
