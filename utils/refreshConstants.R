refreshConstants <- function(cdb,ccc){

  cdb$design <- "constants"
  cdb$view <- "date_value"

  cdb$queryParam <- ""
  
  relConstants <- cdbGetView(cdb)$res
  
  if(length(relConstants$rows) > 0){
    currentConstants <- relConstants$rows[[relConstants$total_rows]]$value$Values
    
    ccc$Calibration$Constants <- currentConstants
  }
  
  return(ccc)

}
