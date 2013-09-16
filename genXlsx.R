## --
## wactbprot/2013-09-05
## --
test       <- FALSE
tmpPath    <- "/tmp/"
extdb      <- "vaclab_ext"

if(!test){
    infList             <- list()
    infList$args        <- commandArgs(TRUE) 
    noOfArgs            <- length(infList$args)
}

if(!test){
    instPath            <- "/usr/local/lib/r4vl/"
    setwd(instPath)
    source("load.R")
    cdb                 <- cdbIni()
    cdb$serverName      <- infList$args[noOfArgs - 2]
    cdb$DBName          <- infList$args[noOfArgs - 1]
    cdb$id              <- infList$args[noOfArgs]

    outdb          <- cdbIni() 
    outdb$DBName   <- extdb
    outdb$id       <- cdb$id
    
}else{
    source("load.R")
    cwd                 <- getwd()
    cdb                 <- cdbIni()
    cdb$DBName          <- "vaclab_db" 
    cdb$id              <- "f481e565fd252673c6a6e7b6b8003f05"
}

doc                 <- cdbGetDoc(cdb)$res
a                   <- abbrevList(doc)
reportName          <- paste(a$cy,a$ct,a$cs,a$csi, sep="-")
outPath             <- paste(tmpPath, reportName,"/", sep="")

setwd(outPath)
## -----------excel-land:
for(structName in c("Measurement","Analysis")){
    xlsxName <- paste(reportName, structName, "xlsx", sep=".")
    if(file.exists(xlsxName)){
        ## immer neu
        file.remove(xlsxName)
    }
    valList  <- a$c[[structName]]$Values
    
    lnames   <- names(valList)
    for(sheetName in lnames){
        
        df <- makeDf(valList[[sheetName]])

        if(file.exists(xlsxName)){
            af = TRUE
        }else{
            af = FALSE
        }
        write.xlsx(df,
                   xlsxName,
                   sheetName=sheetName,
                   append=af)
    }
    if(!test){
        outdb$fileName     <- xlsxName
        resXlsx            <- cdbAddAttachment(outdb)$res
    }
}
##
if(test){
    setwd(cwd)
}

cat(toJSON(list( resXlsx= resXlsx$ok)))

