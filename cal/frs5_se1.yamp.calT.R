frs5_se1.yamp.calT <- function(ccc){
  msg  <- "calculated by frs5_se1.yamp.calT()"
  a    <- abbrevList(ccc)

  TUnit   <- "K"
  C2K     <- getConstVal(a$cc, "C_2_K")

  baseTNameAfter      <- "keithley_T_after_ch"
  baseTNameBefore     <- "keithley_T_before_ch" 
  baseCorrName        <- "keithley_corr_ch" 
  
 
  T         <-  getConstVal(a$cmv$Temperature,"keithley_T_frs5_ch110")
  corrTFrs5 <-  getConstVal(a$cmco,"keithley_corr_ch110")
  Tfrs      <-  T + corrTFrs5
  N         <-  length( Tfrs)
  ## T_after ---------------------------------v
  channelsAfter <- c("103","104","105")
  afterRes <- getTemperatureVec(a,N,channelsAfter,baseTNameAfter,baseCorrName) + C2K
  ## T_after ---------------------------------^
  
  ## T_before ---------------------------------v
  channelsBefore <- c("101")
  resBefore<- getTemperatureVec(a,N,channelsBefore,baseTNameBefore,baseCorrName) + C2K
  ## T_before ---------------------------------^

  ccc$Calibration$Analysis$Values$Temperature <-
    setCcl(ccc$Calibration$Analysis$Values$Temperature,
           "frs5",
           "C",
           Tfrs,
           paste(msg,"correcteed with: ", corrTFrs5 ))
    
    ccc$Calibration$Analysis$Values$Temperature <-
      setCcl(ccc$Calibration$Analysis$Values$Temperature,
             "before",
             TUnit,
             resBefore,
             msg)
    
    ccc$Calibration$Analysis$Values$Temperature <-
      setCcl(ccc$Calibration$Analysis$Values$Temperature,
             "after",
             TUnit,
             afterRes,
             msg)
    
 
  return(ccc)
}
