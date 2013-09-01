source(load.R)

gl <- cdbIni() ## getList
pl <- cdbIni() ##putList

gl$DBName <- "vaclab_db"
gl$id     <- "f481e565fd252673c6a6e7b6b8003f05"

doc <- cdbGetDoc(gl)$res
