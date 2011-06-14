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
cdb$id         <- "81639d04934132e766657369210f2629"
doc <- cdbGetDoc(cdb)$res
doc <- refreshAnalysis(cdb,doc)
doc <- getOutIndex(doc)
doc <- ce3.calT(doc)
doc <- ce3.calPfill(doc)
doc <- ce3.calDvC(doc)
doc <- ce3.calQ(doc)
doc <- ce3.calMfp(doc)
doc <- ce3.writePind(doc)
doc <- ce3.calPcal(doc)






## ----------------------------------------------------
Sys.setenv("no_proxy" = saveNoProxy)
