se1.yamp.calPfill <- function(ccc){
  msg <- "calculated by yamp.calPfill()"
  
  a <- abbrevList( ccc )
  if(a$dataAvailable){
    
    ## -------------------- ruska--------------------------v
    PFILL       <- getSubList(a$cmv, "ruska_p_fill")
    PFILLOFFSET <- getSubList(a$cma, "ruska_p_fill_offset")
    
    omt         <- getConstVal(a$cma, "amt_offset")
    bmt         <- getConstVal(a$cmv, "amt_before")

    if((PFILL$Unit == PFILLOFFSET$Unit) & (PFILL$Unit =="V")){
      
      CA        <-  getSubList(a$cmco, "conv_a")
      CB        <-  getSubList(a$cmco, "conv_b")
      
      if((CA$Unit == "mbar/V") & (CB$Unit == "mbar")){
        
        convA         <-  getConstVal(NA, NA, CA)
        convB         <-  getConstVal(NA, NA, CB)
        
        pfillVolt     <-  getConstVal(NA, NA, PFILL)
        pfillOffVolt  <-  getConstVal(NA, NA, PFILLOFFSET)

        N             <- length(pfillVolt)
        
        if(!is.null(omt) & !is.null(bmt) & 
           (length(omt) > 1) & (length(bmt) > 1)){
          
          offsetVolt <- rep(NA,N)
          
          for(i in 1:length(omt)){
            k             <- which(bmt > omt[i])
            offsetVolt[k] <- pfillOffVolt[i] 
          }
        }else{
          offsetVolt <- pfillOffVolt
        }
        
        pfill <- convA  *  pfillVolt  + convB
        offs  <- convA  *  offsetVolt + convB
        
        msg   <- paste(msg, "calculated with convA: ", convA, "and convB: ",convB)
        pUnit <- "mbar"

        ccc$Calibration$Analysis$Values$Pressure <-
          setCcl(ccc$Calibration$Analysis$Values$Pressure,
                 "fill",
                 pUnit,
                 pfill - offs,
                 msg)
        
        ccc$Calibration$Analysis$Values$Pressure <-
          setCcl(ccc$Calibration$Analysis$Values$Pressure,
                 "fill_uncorr",
                 pUnit,
                 pfill,
                 msg)
        
        ccc$Calibration$Analysis$Values$Pressure <-
          setCcl(ccc$Calibration$Analysis$Values$Pressure,
                 "fill_offset",
                 pUnit,
                 offs,
                 msg)
        
        
      }else{
        stop("conversion of demanded units not implemented (maybe only a typo?)!")
      }
    }else{
      stop("pfill Units dont match")
    }
    ## -------------------- ruska--------------------------^
    return(ccc)
  }
}

