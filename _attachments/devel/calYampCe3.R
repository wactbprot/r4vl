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

## devel/tests/calculations from here -----------------
cdb$id     <- "1f72ec47286b685511b3cc38f0094533"#"1f72ec47286b685511b3cc38f009ceeb"
doc <- cdbGetDoc(cdb)$res

doc <- refreshAnalysis(cdb,doc)

doc <- ce3.newCalPfill(doc)
doc <- ce3.calDeltaVDeltat(doc)

doc <- ce3.extrapC(doc)

#cdb$dataList <- doc
#cdbUpdateDoc(cdb)
## ----------------------------------------------------
Sys.setenv("no_proxy" = saveNoProxy)
