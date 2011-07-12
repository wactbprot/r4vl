calEn <- function(ccc){
  msg <- "calculated by calEnValue()"
  a <- abbrevList(ccc)

  stdVec <- unlist(strsplit(a$cs, "\\|"))

  if(length(stdVec) != 2){
    msg <- paste(msg, "calEn needs 2 Standards (in Calibration.Standard) separated with |")
  }else{
  aType <- tolower(stdVec[1])
  bType <- tolower(stdVec[2])
  pa <- ....
  }

  ccc$Calibration$Analysis$Values$En <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure, "p_ind",
           indUnit,
           ind,
           paste(msg)
            )

  return(ccc)
}
