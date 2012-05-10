## --
## wactbprot/2012-05-10
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

setwd("/home/bock04/eig/map/_attachments/devel/")
dataPath <- "/home/bock04/eig/FM1/data/FM1-leitwert.csv"

## ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ special FNs start:
## iniMeas <- function(cdb,ccc){
##   ## für die Uns. Berechnung wird eine
##   ## template Messung erzeugt
##   ccc$Calibration$Measurement <- list()
##   ccc$Calibration$Measurement$Values <- list()
##   
##   ccc$Calibration$Measurement$Maintainer <- toString(Sys.getenv("USER"))
##   ccc$Calibration$Measurement$Date   <-
##     list(
##       Type="generated",
##       Value=toString(format(Sys.time(), "%Y-%m-%d" ) )
##       )
##   
##   return( ccc )
## }
## ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ special FNs end

writeData <- TRUE
## daten werden für das entw. der unsicher-
## heitsfunktionen in das calDoc geschrieben
## (muss nur 1mal geschehen, daten müssen nur vorh. sein)


if(writeData){
  dat <- as.list(read.csv(dataPath, sep=";", dec=","))

  cdb$id <- "2f63d987154f6a4896ed7016510006b0"
  ccc <-  cdbGetDoc(cdb)$res
  ##  ccc <-  iniMeas(cdb,ccc)
  ccc <-  refreshAnalysis(cdb,ccc)

  C2K <- getConvFactor(ccc,"K","C")

  mm2m <- getConvFactor(ccc,"m","mm")

  ccc$Calibration$Analysis$Values$Length <-
    setCcl(ccc$Calibration$Analysis$Values$Length,
           "dl",
           "m",
           dat$distance * mm2m,
           "Verfahrweg pro SZ")
  
   ccc$Calibration$Analysis$Values$Length <-
    setCcl(ccc$Calibration$Analysis$Values$Length,
           "l",
           "m",
           (dat$distance * mm2m)*(dat$i.SZ +1),
           "Verfahrweg insgesammt (dat$distance * mm2m)*(dat$i.SZ +1)")
  
  ccc$Calibration$Analysis$Values$Temperature <-
    setCcl(ccc$Calibration$Analysis$Values$Temperature,
           "room",
           "K",
           dat$T.Raum.kor + C2K,
           paste("corrected room temperature source dat$T.Raum.kor + C2K file: ",dataPath))

  ccc$Calibration$Analysis$Values$Temperature <-
    setCcl(ccc$Calibration$Analysis$Values$Temperature,
           "fm1",
           "K",
           dat$T.FM.kor + C2K,
           paste("corrected temperature of flowmeter  source dat$T.FM.kor + C2K file: ",dataPath))
  
  ccc$Calibration$Analysis$Values$Conductance <- 
    setCcl(ccc$Calibration$Analysis$Values$Conductance,
           "cfm1",
           "l/s",
           dat$L,
           paste("conductance source dat$L file: ",dataPath) )
  
  ccc$Calibration$Analysis$Values$Pressure <- 
    setCcl(ccc$Calibration$Analysis$Values$Pressure,
           "fill",
           "mbar",
           dat$p.korrigiert,
           paste("corrected fill pressure source  dat$p.korrigiert file: ",dataPath))
}



ccc <- fm1.uncertPfill(ccc)
ccc <- fm1.uncertDPfill(ccc)
ccc <- fm1.uncertPres(ccc)
ccc <- fm1.uncertDeltaV(ccc)
ccc <- fm1.uncertDeltat(ccc)

ccc <- fm1.uncertTfm(ccc)
ccc <- fm1.uncertCmol(ccc)

ccc <- fm1.uncertqpV(ccc)

#cdb$dataList <- doc
#Result <-  cdbUpdateDoc(cdb)$res

plot(dat$p.korrigiert,
     getConstVal(ccc$Calibration$Analysis$Values,"uncertqpV"),
     log="xy",
     ylim=c(1e-5, 1e-2),
     type="b")

points(dat$p.korrigiert,
       getConstVal(ccc$Calibration$Analysis$Values,"uncertCmol"),
       col=2,
     type="b")

points(dat$p.korrigiert,
       getConstVal(ccc$Calibration$Analysis$Values,"uncertTfm"),
       col=3,
     type="b")

points(dat$p.korrigiert,
       getConstVal(ccc$Calibration$Analysis$Values,"uncertDeltat"),
       col=4,
     type="b")

points(dat$p.korrigiert,
       getConstVal(ccc$Calibration$Analysis$Values,"uncertDeltaV"),
       col=5,
     type="b")

points(dat$p.korrigiert,
       getConstVal(ccc$Calibration$Analysis$Values,"uncertPres"),
       col=6,
     type="b")

points(dat$p.korrigiert,
       getConstVal(ccc$Calibration$Analysis$Values,"uncertDPfill"),
       col=7,
     type="b")

points(dat$p.korrigiert,
       getConstVal(ccc$Calibration$Analysis$Values,"uncertPfill"),
       col=8,
     type="b")

legend(mean(dat$p.korrigiert),
       5e-4,
       c(
         "uncertqpV",
         "uncertCmol",
         "uncertTfm",
         "uncertDeltat",
         "uncertDeltaV",
         "uncertPres",
         "uncertDPfill",
         "uncertPfill"
         ),
       col=1:8,
       lwd=rep(1,8))
grid(col=1)
