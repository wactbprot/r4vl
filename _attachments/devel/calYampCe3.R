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
cdb$id     <- "6f9efbc2c556711153e5c70d8e000b81"#"aeeb98402d18d122fde902dbc3005c95"
doc <- cdbGetDoc(cdb)$res

doc <- refreshAnalysis(cdb,doc)

doc <- ce3.newCalPfill(doc)
doc <- ce3.calDeltaVDeltat(doc)

doc <- ce3.extrapC(doc)

#cdb$dataList <- doc
#cdbUpdateDoc(cdb)
## ----------------------------------------------------
Sys.setenv("no_proxy" = saveNoProxy)
