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
cdb$id         <- "b3a62dbf073c36cd5a833ad3ee002a98"
doc <- cdbGetDoc(cdb)$res

doc <- uncertQsplit(doc)
	 

doc <- ce3.uncertCx(doc)
doc <- ce3.uncertDPfill(doc)
doc <- ce3.uncertDeltaV(doc)
doc <- ce3.uncertDeltaVDeltat(doc)
doc <- ce3.uncertDeltat(doc)
doc <- ce3.uncertFmol(doc)
doc <- ce3.uncertPfill(doc)
doc <- ce3.uncertQsplit(doc)
doc <- ce3.uncertTfm(doc)
doc <- ce3.uncertqpV(doc)
doc <- ce3.uncertCmol(doc) 

## ----------------------------------------------------
Sys.setenv("no_proxy" = saveNoProxy)
