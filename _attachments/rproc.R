## --
## wactbprot/2011-05-25
## --
library(methods,  quietly =TRUE)
library(bitops,   quietly =TRUE)
library(RJSONIO,  quietly =TRUE)
library(RCurl,    quietly =TRUE)
library(R4CouchDB,quietly =TRUE) ## ---> encoding!
## it's normally enough to load my R4Couchdb
## but if one realy don't like to
## hear a sound
## (as the http_db_handler demands; no print no cat!),
## one have to say:
## quietly =TRUE
## for all libs loaded
## maybe there is a way to
## express this?

goOn    <- TRUE
loadSrc <- TRUE
NL <- "\n"
cdb  <- cdbIni()

## I guess a bug in src:
## also with:
##    Sys.getenv("no_proxy")
##    [1] "localhost, 127.0.0.1"
## source search localhost over
## the proxy!
## Since we have a local R
## switching off all proxys
## seems to be ok
saveNoProxy <- Sys.getenv("no_proxy")
Sys.setenv("no_proxy" = "*")

while(goOn){

  jIn <- scan('stdin',
              '',
              n=1,
              quiet=TRUE,
              sep=NL)

  rIn    <- fromJSON(jIn)

  query  <- rIn$query
  qLen   <- length(query) > 0

  cdb$DBName <- rIn$path[[1]]
  ## at rIn$path[[2]] we have the "_rproc" part
  ##    rIn$path[[3]] is the id, we use it   -->here

  if(qLen){
    if(length(query$ctrl) == 1){
      switch(query$ctrl,
             "end"     = goOn    <- FALSE,
             "reload"  = loadSrc <- TRUE
             ## whatever ...
             )
    }
  }
  ## loads the src files from couch
  ## maybe the id of the design doc
  ## varies
  if(loadSrc){
    cdb$id <- paste("_design/rproc",sep="")
    srcDoc <- cdbGetDoc(cdb)$res
    files <- names(srcDoc$'_attachments')

    baseSrcUrl <- paste(cdb$baseUrl(cdb)
                        ,cdb$DBName,"/",
                        cdb$id,"/",
                        sep="")

    ## in the folder R under _attachment folder
    ## of the couchapp one can put functions
    ## which will be available for use when the
    ## a script execution is demanded
    ## (via http://server:5984/db/_rproc/id?script=scriptname)
    for(file in  files){
      fn <- grep("^R/.*\\.R$",file)

      if(length(fn) > 0){
        srcUrl <- paste(baseSrcUrl,
                        file,
                        sep="")

        source(srcUrl)

      }
    }
    loadSrc <- FALSE
  } # load src

  ## at this place the src (functions under _attachment/R/)
  ## is loaded
  ## we can do whatever we want
  ## e.g. getting the doc
  ## the id is gained  from the calling url
  ##                                        -->here
  cdb$id   <- rIn$path[[3]] #
  ## ----------- the querys
  if(qLen){
    if(length(query$script) == 1){



      iscript <-  grep(query$script,
                       files)
      if(length(iscript) == 1){

        doc <- cdbGetDoc(cdb)$res
        ## do stuff ----------
        source(paste(baseSrcUrl,
                     files[iscript],
                     sep=""))
        ## stuff done -------
        cdb$dataList <- doc
        Result <-  cdbUpdateDoc(cdb)$res

      }else{
        Result <- "script name found 0 or >1 times"
        }
      }
    }else{
  Result <- "No query"
  }

  rout <- list(code = 200,
               json=list(Result=Result))

  jOut <- gsub(NL,"",toJSON(rout))

  cat( paste(jOut,
             NL, sep="")
      )

}
Sys.setenv("no_proxy" = saveNoProxy)
### another methode to get the src:
##- need to get the script path
##- this can be found at the config api end point
## cfg  <- cdbGetConfig(cdb)$res
##  pathspl <- unlist(strsplit(cfg$external$rproc, " "))
##  uPath <- sub("rproc.R",'',pathspl[length(pathspl)] )
##- from here:
##  setwd(uPath)
##- the wd is the same
##- as given in local.ini of couchdb

### this can be done for debug:
## save(rIn,file="path_to_somewhere/a.oo",
## ascii=TRUE)


