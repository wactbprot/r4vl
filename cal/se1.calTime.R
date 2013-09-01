se1.calTime <- function(ccc){
  msg <- "calculated by se1.calTime()"

  a <- abbrevList( ccc )


  rmtBEFORE       <- getSubList(a$cmv, "rmt_before")
  rmtAFTER        <- getSubList(a$cmv, "rmt_after")

  if(!is.null(rmtBEFORE) & !is.null(rmtAFTER)){
    if(rmtBEFORE$Unit == "s"){
      
      rmtBefA.h <- getConstVal(NA,NA,rmtBEFORE)/3600

      ccc$Calibration$Analysis$Values$Time <-
        setCcl(ccc$Calibration$Analysis$Values$Time, "rmt_before",
               "h",
               rmtBefA.h,
               msg)
      
    }
    
    if(rmtAFTER$Unit == "s"){
      
      rmtAfterA.h <- getConstVal(NA,NA,rmtAFTER)/3600
      
      ccc$Calibration$Analysis$Values$Time <-
        setCcl(ccc$Calibration$Analysis$Values$Time, "rmt_after",
               "h",
               rmtAfterA.h,
               msg)

    }
  }


  amtBEFORE       <- getSubList(a$cmv, "amt_before")
  amtAFTER        <- getSubList(a$cmv, "amt_after")
  ## todo:
  ## aus measurement date anfangszeit ber.
  ## dann rmt in min nach Analysis
  ##


  
  return(ccc)
}
