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

fn <- function(cf, x){
  r <- cf$a0 + cf$a1*x + cf$a2*x^2 + cf$a3*x^3 +cf$a4*x^4 +cf$a5*x^5 
  return(r)
}



data <- ce3.getConductanceHist(cdb)

dvSep <- 9e-6
P <- unlist(data$pfill)
C <- unlist(data$conductance)
D <- unlist(data$date)

gas   <- unlist(data$gas)

igrLw  <- which(C > dvSep)
iklLw  <- which(C < dvSep)

iN2    <- which(gas == "N2")
iAr    <- which(gas == "Ar")

## edit --------------------
i1 <- which(igrLw %in% iAr)

cp <- C[igrLw[i1]]
pp <- P[igrLw[i1]]
dd <- D[igrLw[i1]]
xlab  <- "p in mbar"
ylab  <- "C in l/s"
main  <- "Ar/gr. LW"

baseName <- "grLwAr"
pdfname <- paste(baseName,".pdf", sep="")
datname <-  paste(baseName,".dat", sep="")
## -------------------------
setwd("/home/bock04/eig/map/_attachments/datasets")
write(rbind(pp,cp),datname, ncol=2)

setwd("/home/bock04/eig/map/_attachments/diag")
pg <- seq(min(pp), max(pp))

lf  <- lm(cp ~ poly(pp, 4, raw=TRUE))
cf <- list(a0 = coef(lf)[1],
           a1 = coef(lf)[2],
           a2 = coef(lf)[3],
           a3 = coef(lf)[4],
           a4 = coef(lf)[5],
           a5 = 0)#coef(lf)[6])

sub <- paste("\n",
             "a0: ",signif(cf$a0,digits = 5)
             ,",",
             "a1: ",signif(cf$a1,digits = 5)
             ,",",
             "a2: ",signif(cf$a2,digits = 5)
             ,",","\n",
             "a3: ",signif(cf$a3,digits = 5)
             ,",",
             "a4: ",signif(cf$a4,digits = 5)
             ,",",
             "a5: ",signif(cf$a5,digits = 5)
)

pdf(pdfname)
par(mfrow=c(2,1))
plot(pp,cp, log="x", xlab=xlab, ylab=ylab, main=main)
points(pg,fn(cf,pg), type="l", col=2)
grid(col=1)
plot(pp,lf$residuals/cp,
     log="x",
     main=sub)
grid(col=1)
dev.off()

