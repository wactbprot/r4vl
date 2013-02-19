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

cdb$design <- "map"
cdb$view   <- "ce3-get_flow_by_date"

## Zeitraum ist:
## vom 2009-12-08 bis zum 2012-01-25
##
cdb$queryParam <- 'startkey="2009-12-16"&endkey="2013-01-25"'
res            <- cdbGetView(cdb)$res

resPfill <- NULL
resCond  <- NULL
resTemp  <- NULL
resDate  <- NULL
resGas   <- NULL

for(i in 1:length(res$rows)){

  tmpCond  <- getSubList(res$rows[[i]]$value$Conductance, "cfm3")$Value
  lc       <- length(tmpCond)
  tmpPfill <- getSubList(res$rows[[i]]$value$Pressure, "fill")$Value
  tmpTemp  <- getSubList(res$rows[[i]]$value$Temperature, "Tfm3")$Value

  if (length(tmpPfill) ==lc &
      length(tmpTemp)  ==lc &
      lc >0){

    tmpDate <- rep(res$rows[[i]]$value$Date,lc)
    tmpGas  <- rep(res$rows[[i]]$value$Gas,lc)

    resPfill <- append(resPfill,tmpPfill)
    resCond  <- append(resCond,tmpCond)
    resTemp  <- append(resTemp,tmpTemp)
    resDate  <- append(resDate,tmpDate)
    resGas   <- append(resGas,tmpGas)
  }
}


dvSep <- 9e-6

igrLw  <- which(resCond > dvSep)
iklLw  <- which(resCond < dvSep)

iN2    <- which(resGas == "N2")
iAr    <- which(resGas == "Ar")
iD2    <- which(resGas == "D2")

plot(resPfill[iD2], resCond[iD2])


## edit --------------------
i1 <- which(igrLw %in% iD2)
m  <- igrLw[i1]

baseName <- "grLwD2"

## -------------------------

datname <-  paste(baseName,".dat", sep="")

setwd("/home/bock04/eig/map/_attachments/datasets")
write(rbind(
        unlist(resCond[m]),
        unlist(resPfill[m]),
        unlist(resDate[m]),
        unlist(resTemp[m]),
        unlist(resGas[m])),
      datname,
      ncol=5)


