calEn <- function(ccc){
  msg <- "calculated by calEnValue()"
  a <- abbrevList(ccc)

  stdVec <- unlist(strsplit(a$cs, "\\|"))

  if(length(stdVec) != 2){
    msg <- paste(msg, "the current implementation of calEn needs 2 Standards (in Calibration.Standard) separated with |")
  }else{
    aType <- tolower(stdVec[1])
    bType <- tolower(stdVec[2])

    PA <- getSubList(a$cav$Pressure, aType)
    unit <- PA$Unit

    pa <- getConstVal(a$cav$Pressure, aType)
    pb <- getConstVal(a$cav$Pressure, bType)

    ua <- getConstVal(a$cav$Uncertainty, paste("u_",aType,sep=""))
    ub <- getConstVal(a$cav$Uncertainty, paste("u_",bType,sep=""))

    d <- pa - pb
    U <- 2*(ua^2+ub^2)^.5
    wmv <- (pa/ua^2+pb/ub^2)/(1/ua^2+1/ub^2)
    En <- abs(d/U)


    ccc$Calibration$Analysis$Values$En <-
    setCcl(ccc$Calibration$Analysis$Values$En, "en",
           "1",
           En,
           paste("En=|d/U|",msg)
           )

    ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure, "d",
           unit,
           d,
           paste("d=p_std1-p_std2",msg)
           )

    ccc$Calibration$Analysis$Values$Pressure <-
    setCcl(ccc$Calibration$Analysis$Values$Pressure, "mv_w",
           unit,
           wmv,
           paste("mv_w = (p_std1/u_std1^2 + p_std2/u_std2^2(/(1/u_std1^2 + 1/u_std2^2 ) ",msg)
           )
  }

  return(ccc)
}
