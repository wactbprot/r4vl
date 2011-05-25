uncertIni <- function(servername="a73435.berlin.ptb.de",
                      dbname = "thomas_db",
                      id="4bf410facb1f0dc75a61fa8fa35318f6"){
library(R4CouchDB)


cdb <- cdbIni()
cdb$serverName <- servername
cdb$DBName <- dbname
### Testdoc
cdb$id <- id

ccc <- cdbGetDoc(cdb)$res
ccc <- checkPresettings(cdb,ccc)
ccc <- updateCalibration(cdb,ccc)
ccc <- updateStandard(cdb,ccc)
ccc <- updateConstants(cdb,ccc)


return(list(ccc=ccc,
            cdb=cdb))
}
