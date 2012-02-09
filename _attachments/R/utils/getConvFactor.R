getConvFactor <- function(ccc,toUnit,fromUnit){
  ##              zBsp.:       ^ cal     ^ind
  ## funktioniert nur wenn ConvFaktoren vorhanden und nach der Form
  ## mbar_2_Pa
  ## codiert sind
  a            <- abbrevList(ccc)
  conv         <- 1
  
  if(is.list(fromUnit)){
    fromUnit   <- fromUnit$Unit
  }
  if(is.list(toUnit)){
    toUnit     <- toUnit$Unit
  }

  if(toUnit == fromUnit){
    conv       <- 1
  }else{
    convString <- paste(fromUnit, "_2_",toUnit, sep="")
    conv       <-  getConstVal(a$cc$Conversion,convString)
  }

  if(length(conv) == 1){
    return(conv)
  }else{
    stop("no conversion factor found")
  }
}
