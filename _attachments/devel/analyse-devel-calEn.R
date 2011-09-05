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

  cdb$id <- paste("_design/rproc",sep="")
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

## devel/tests/calculations from here -----------------
cdb$id         <- "c57a5943a80348e1e6489e68fc03e95d"

doc <- cdbGetDoc(cdb)$res
doc <- refreshAnalysis(cdb,doc)
doc <- getOutIndex(doc)

doc <- dkm.calPdkm(doc)
doc <- frs5.calPfrs5(doc)

doc <- dkm.uncertPdkm(doc)
doc <- frs5.uncertPfrs5(doc)

doc <- calEn(doc)



## ----------------------------------------------------
Sys.setenv("no_proxy" = saveNoProxy)
