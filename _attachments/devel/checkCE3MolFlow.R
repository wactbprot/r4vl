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




data <- ce3.getMolFlowHist(cdb)

dvSep <- 9e-6
P <- unlist(data$pfill)
C <- unlist(data$conductance)

gas   <- unlist(data$gas)

igrLw  <- which(C > dvSep)
iklLw  <- which(C < dvSep)

iN2    <- which(gas == "N2")
iAr    <- which(gas == "Ar")


i1 <- which(igrLw %in% iN2)

cp <- C[igrLw[i1]]
pp <- P[igrLw[i1]]

lm(cp ~ poly(pp, 4)/poly(pp, 4))



