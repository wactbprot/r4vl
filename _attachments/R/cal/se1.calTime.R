se1.calTime <- function(ccc){
  msg <- "calculated by se1.calTime()"
  
  a <- abbrevList( ccc )
  
  
  rmtBEFORE       <- getSubList(a$cmv, "rmt_before")
  rmtAFTER        <- getSubList(a$cmv, "rmt_after")
  
  if(rmtBEFORE$Unit == "s"){
    
    rmtBefA.h <- getConstVal(NA,NA,rmtBEFORE)/3600 

    rmtBefA.h  <- checkOutIndex(a,rmtBefA.h)

    ccc$Calibration$Analysis$Values$Time <-
      setCcl(ccc$Calibration$Analysis$Values$Time, "rmt_before",
             "h",
             rmtBefA.h,
             msg)
    
  }
  
  if(rmtAFTER$Unit == "s"){
    
    rmtAfterA.h <- getConstVal(NA,NA,rmtAFTER)/3600 

    rmtAfterA.h  <- checkOutIndex(a,rmtAfterA.h)
    
    ccc$Calibration$Analysis$Values$Time <-
      setCcl(ccc$Calibration$Analysis$Values$Time, "rmt_after",
             "h",
             rmtAfterA.h,
             msg)
    
  }
  
  
  return(ccc)
}
