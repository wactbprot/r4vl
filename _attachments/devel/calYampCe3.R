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
#"1f72ec47286b685511b3cc38f0094533"#"1f72ec47286b685511b3cc38f009ceeb"
cdb$id     <- "1f72ec47286b685511b3cc38f00a90a2"#
doc <- cdbGetDoc(cdb)$res
doc <- refreshAnalysis(cdb,doc)

doc <- ce3.newCalPfill(doc)
doc <- ce3.calDeltaVDeltat(doc)
doc <- ce3.extrapC(doc)
doc <- ce3.newCalT(doc)
doc <- ce3.calQ(doc)
doc <- ce3.calMfp(doc)
doc <- ce3.writePind(doc)
doc <- ce3.calPcal(doc)
doc <- dispatchResCal(doc)
doc <- fm3.uncertPfill(doc)
doc <- fm3.uncertDPfill(doc)
doc <- fm3.uncertDeltaV(doc)
doc <- fm3.uncertDeltaVDeltat(doc)
doc <- fm3.uncertDeltat(doc)
doc <- fm3.uncertFmol(doc)
doc <- fm3.uncertqpV(doc)
doc <- fm3.uncertCmol(doc)
doc <- ce3.uncertCx(doc)
doc <- ce3.uncertQsplit(doc)
doc <- ce3.uncertTfm(doc)
doc <- ce3.uncertTch(doc)
doc <- ce3.uncertF(doc)
doc <- ce3.uncertPcal(doc)

                                        #cdb$dataList <- doc
                                        #cdbUpdateDoc(cdb)
## ----------------------------------------------------
Sys.setenv("no_proxy" = saveNoProxy)
