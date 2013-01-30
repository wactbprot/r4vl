se1.yamp.calPfill <- function(ccc){
  msg <- "calculated by yamp.calPfill()"
  
  a <- abbrevList( ccc )
  if(a$dataAvailable){
    
    ## -------------------- ruska--------------------------v
    PFILL       <- getSubList(a$cmv, "ruska_p_fill")
    PFILLOFFSET <- getSubList(a$cmv, "ruska_p_fill_offset")
    
    omt         <- getConstVal(a$cma, "amt_offset")
    
    if((PFILL$Unit == PFILLOFFSET$Unit) & (PFILL$Unit =="V")){
      
      CA        <-  getSubList(a$cmco, "conv_a")
      CB        <-  getSubList(a$cmco, "conv_b")
      
      if((CA$Unit == "mbar/V") & (CB$Unit == "mbar")){
        
        convA               <-  getConstVal(NA, NA, CA)
        convB               <-  getConstVal(NA, NA, CB)
        
        pfillVolt     <-  getConstVal(NA, NA, PFILL)
        pfillOffVolt  <-  getConstVal(NA, NA, PFILLOFFSET)

        if(length(omt) > 1){



        }




        
        pfill <- convA  *  (pfillVolt - pfillOffVolt) + convB
        
        msg   <- paste(msg, "calculated with:", convA, "* (pfill - pfillOffs) +",convB)
        pUnit <- "mbar"
        
      }else{
        stop("conversion of demanded units not implemented (maybe only a typo?)!")
      }
    }else{
      stop("pfill Units dont match")
    }
    ## -------------------- ruska--------------------------^

    pfill <- pfill

    ccc$Calibration$Analysis$Values$Pressure <-
      setCcl(ccc$Calibration$Analysis$Values$Pressure,
             "fill",
             pUnit,
             pfill,
             paste(msg, "p_fill = pfill"))

        
    return(ccc)
  }
}

