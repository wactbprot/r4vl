updateCalibration <- function(cdb,ccc){

  ccc <- checkPresettings(cdb,ccc)

  ccc<-  refreshStandard(cdb,ccc)
  ccc<-  refreshAnalysis(cdb,ccc)
  ccc<-  refreshConstants(cdb,ccc)
  ccc<-  refreshCustomerCalibrationObject(cdb,ccc)
  ccc<-  refreshCalibrationObject(cdb,ccc)

  return(ccc)

}



