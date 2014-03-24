## --
## wactbprot/2013-09-05
## --
test       <- FALSE
tmpPath    <- "/tmp/"
extdb      <- "vaclab_ext"
futonStr   <- "http://a73434.berlin.ptb.de:5984/_utils/document.html?vaclab_db/"
valTypes   <- c("Measurement","Analysis")
instPath   <- "/usr/local/lib/r4vl/"

if(!test){
    ##
    infList             <- list()
    infList$args        <- commandArgs(TRUE) 
    noOfArgs            <- length(infList$args)
    
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

measDates           <- toString(unlist(a$cm$Date[[1]]$Value))
reportName          <- paste(a$cy,a$ct,a$cs,a$csi, sep="-")
outPath             <- paste(tmpPath, reportName,"/", sep="")

setwd(outPath)


## -*- excel-land:
for(structName in valTypes){
    xlsxName <- paste(reportName, structName, "xlsx", sep=".")
    
    ## immer neue files
    if(file.exists(xlsxName)){
        file.remove(xlsxName)
    }
    ## head data frame
    headDf <- data.frame(id       = doc[["_id"]],
                         rev      = doc[["_rev"]],
                         sign     = a$csi,
                         type     = a$ct,
                         year     = a$cy,
                         measDate = measDates,
                         futonUrl = paste(futonStr, doc[["_id"]],sep="")) 
    ## head schreiben
    write.xlsx(t(headDf),
               xlsxName,
               sheetName="Header")
    
    valList  <- a$c[[structName]]$Values
    
    lnames   <- names(valList)
    for(sheetName in lnames){
        
        df <- makeDf(valList[[sheetName]])
        ## weil head schon geschrieben,
        ## kann immer angehangen werden
        write.xlsx(df,
                   xlsxName,
                   sheetName=sheetName,
                   append=TRUE)
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

## delete folder
setwd("..")
unlink( outPath,recursive=TRUE)

cat(toJSON(list( resXlsx= resXlsx$ok)))
