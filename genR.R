source("load.R")

gl <- cdbIni() ## getList
pl <- cdbIni() ##putList

gl$DBName <- "vaclab_db"
gl$id     <- "910fd907311a80c24cee9ad18a07d57b"

doc <- cdbGetDoc(gl)$res
a   <- abbrevList(doc)

if(a$cs == "CE3"){
    knit("html/ce3.report.Rhtml")
}
