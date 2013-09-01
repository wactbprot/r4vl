refreshStandard <- function(cdb,ccc){

  cdb$design <- "standard_ce3"
  cdb$view <- "get_standard"

  relStandard <- cdbGetView(cdb)$res

  if(length(relStandard$rows) > 0){

    currentStandard <- relStandard$rows[[relStandard$total_rows]]$value$Values
    ## hier könnte das Standard doc noch überprüft werden
    ccc$Calibration$Measurement$Standard <- currentStandard
  }

  return(ccc)
}
