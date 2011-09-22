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
NL <- "\n"

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
cdb$id         <- "6cd06157eef6d60bce05ad2a11086521"
ccc <- cdbGetDoc(cdb)$res

 if(length(ccc$Calibration) > 0){
 
   ccc <- refreshAnalysis(cdb,ccc)

 
 
   ccc <- frs5.calPfrs5(ccc)
   ccc <- frs5.uncertPfrs5(ccc)
   ccc <-  frsbs.calPfrsbs(ccc)
   ccc <-  calDFrs5FrsBs(ccc)
   
   cdb$dataList <- ccc
 
    res <- cdbUpdateDoc(cdb)$res
 }

 rout <- list(code = 200,
               json=list(Result=res))

  jOut <- gsub(NL,"",toJSON(rout))

  cat( paste(jOut,
             NL, sep="")
      )



## ----------------------------------------------------
Sys.setenv("no_proxy" = saveNoProxy)
