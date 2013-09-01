se1.calPfill <- function(ccc){
  msg <- "calculated by calPfill()"
  
  a <- abbrevList( ccc )
  if(a$dataAvailable){
    ## a$cmscex steht für SequenceControll/Expansion
    N <- length(a$cmscex)
    ## es muss jetzt ein resVec mit der Länge N gebildet werden,
    ## der mit cdg oder ruska werten gefüllt wird
    ## zum schluss muss noch nach verbleibenden NAs getested werden
    
    ## -------------------- ruska--------------------------v
    ruska.PFILL       <- getSubList(a$cmv, "ruska_p_fill")
    ruska.PFILLOFFSET <- getSubList(a$cmv, "ruska_p_fill_offset")
    
    if((ruska.PFILL$Unit == ruska.PFILLOFFSET$Unit) & (ruska.PFILL$Unit =="V")){
      
      CA        <-  getSubList(a$cmco, "conv_a")
      CB        <-  getSubList(a$cmco, "conv_b")
      
      if((CA$Unit == "mbar/V") & (CB$Unit == "mbar")){
        
        convA        <-  getConstVal(
                                     NA, NA, CA)
        convB        <-  getConstVal(NA, NA, CB)
        
        ruska.pfillVolt    <-  getConstVal(NA, NA, ruska.PFILL)
        ruska.pfillOffVolt <-  getConstVal(NA, NA, ruska.PFILLOFFSET)
        ruska.pfillVolt <- checkOutIndex(a,ruska.pfillVolt)
        ruska.pfillOffVolt <- checkOutIndex(a,ruska.pfillOffVolt)

        ruska.pfill <- convA  *  (ruska.pfillVolt - ruska.pfillOffVolt) + convB
        
       
        
        msg   <- paste(msg, "calculated with:", convA, "* (pfill - pfillOffs) +",convB)
        pUnit <- "mbar"
        
      }else{
        stop("conversion of demanded units not implemented (maybe only a typo?)!")
      }
    }else{
      stop("pfill Units dont match")
    }
    ## -------------------- ruska--------------------------^
      
    ## -------------------- cdg--------------------------v
    cdg.PFILL       <- getSubList(a$cmv, "cdg_p_fill")
    cdg.PFILLOFFSET <- getSubList(a$cmv, "cdg_p_fill_offset")
    
    if((cdg.PFILL$Unit == cdg.PFILLOFFSET$Unit) & (cdg.PFILL$Unit =="V")){
      ## korrektur bezieht sich auf:
      ## e(%)=(a+c*pind+e*pind^2+g*pind^3)/(1+b*pind+d*pind^2+f*pind^3) 
      
      CDG.A   <-  getSubList(a$cmco, "cdg_corr_a")
      CDG.B   <-  getSubList(a$cmco, "cdg_corr_b")
      CDG.C   <-  getSubList(a$cmco, "cdg_corr_c")
      CDG.D   <-  getSubList(a$cmco, "cdg_corr_d")
      CDG.E   <-  getSubList(a$cmco, "cdg_corr_e")
      CDG.F   <-  getSubList(a$cmco, "cdg_corr_f")
      CDG.G   <-  getSubList(a$cmco, "cdg_corr_g")
      
      if( CDG.A$Unit == "%" &
         CDG.B$Unit == "%/V" &
         CDG.C$Unit == "%/V" &
         CDG.D$Unit == "%/V^2" &
         CDG.E$Unit == "%/V^2" &
         CDG.F$Unit == "%/V^3" &
         CDG.G$Unit == "%/V^3" ){
        
        cdg.pfillVolt <- getConstVal(NA, NA,cdg.PFILL) - getConstVal(NA, NA,cdg.PFILLOFFSET)
        
        cdg.a <- getConstVal(NA, NA, CDG.A)
        cdg.b <- getConstVal(NA, NA, CDG.B)
        cdg.c <- getConstVal(NA, NA, CDG.C)
        cdg.d <- getConstVal(NA, NA, CDG.D)
        cdg.e <- getConstVal(NA, NA, CDG.E)
        cdg.f <- getConstVal(NA, NA, CDG.F)
        cdg.g <- getConstVal(NA, NA, CDG.G) 
        
        
        cdg.pfillVolt    <- checkOutIndex(a,cdg.pfillVolt)
       
        
        
        cdg.error <- (cdg.a + cdg.c * cdg.pfillVolt + cdg.e * cdg.pfillVolt^2 + cdg.g * cdg.pfillVolt^3)/ 
          (1 + cdg.b * cdg.pfillVolt + cdg.d * cdg.pfillVolt^2 + cdg.f * cdg.pfillVolt^3) 
        
        cdg.pfill <-  cdg.pfillVolt/(cdg.error/100 + 1)
        
      }else{
        stop("problen with units of cdg correction in se1.calPfill()") 
      }## Korrektur Units
    }else{
      stop("conversion of demanded units not implemented (maybe only a typo?)!")
    }
    ## -------------------- cdg--------------------------^

    pfill <- ruska.pfill
    
    dpfill <- cdg.pfill/ruska.pfill -1
    
    io <- which(is.na(dpfill))
    if(length(io) > 0){
      dpfill[io] <- 0
    }
    
    
    ccc$Calibration$Analysis$Values$Pressure <-
      setCcl(ccc$Calibration$Analysis$Values$Pressure,
             "fill",
             pUnit,
             ruska.pfill,
             paste(msg, "p_fill = ruska.pfill"))

     ccc$Calibration$Analysis$Values$Pressure <-
       setCcl(ccc$Calibration$Analysis$Values$Pressure,
              "delta_fill",
              "1",
              dpfill,
              "comparison between ruska and cdg with p_cdg/p_ruska - 1, NANs are replaced by 0 until Duncan answers my bug report")
     
    return(ccc)
  }
}

