checkPresettings <- function(cdb,ccc){

  a <- abbrevList(ccc)

  ccc$Calibration$Presettings <- checkSetList(a$cp)
  ccc$Calibration$Presettings$ToDo <- checkSetList(a$cpt)

  return( ccc )
}
