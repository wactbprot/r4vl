ce3.writePind <- function(ccc){
  msg <- "written by ce3.writePind()"
  a     <- abbrevList(ccc)

  pUnit <- "mbar"

  if(length(a$cpt)>0){
    if(length(a$cpt$Pressures)>0){
      if(length(a$cpt$Pressures$Unit)>0){
        pUnit <- a$cpt$Pressures$Unit
      }
    }
  }

  ## the tribut to god of schemeless design
  IND <- getSubList(a$cm, "p_ind")
  
  if(is.null(IND)){
    IND <- getSubList(a$cm, "ind")
  }
  
  OFF <- getSubList(a$cm, "p_ind_offset")
  if(is.null(OFF)){
    OFF <- getSubList(a$cm, "ind_offset")
  }
  
  if(is.null(OFF)){
    OFF <- getSubList(a$cm, "offset")
  }
 
  if(!is.null(IND) && !is.null(OFF) && IND$Unit != "DCR"){

    pind <- getConstVal(NA,NA,IND) * getConvFactor(ccc,pUnit,IND$Unit)
    poff <- getConstVal(NA,NA,OFF) * getConvFactor(ccc,pUnit,OFF$Unit)
    pindcorr <- pind - poff

    if(length(a$cmscoi) > 0){
      if(a$cmscoi[1] > 0){
        pindcorr <- pindcorr[-a$cmscoi]
      }
    }
    ccc$Calibration$Analysis$Values$Pressure <- setCcl(ccc$Calibration$Analysis$Values$Pressure,
                                                     "corrind",
                                                       pUnit,
                                                       pindcorr,
                                                       msg)
  }
  return(ccc)
}
