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



  IND <- getSubList(a$cmv, "ind")
  if(!is.null(IND)){
    OFF <- getSubList(a$cmv, "ind_offset")

    pind <- getConstVal(NA,NA,IND) * getConvFactor(ccc,pUnit,IND$Unit)
    poff <- getConstVal(NA,NA,OFF) * getConvFactor(ccc,pUnit,OFF$Unit)
    pindcorr <- pind - poff

    if(length(a$cmscoi) > 0){
      if(a$cmscoi[1] > 0){
        pindcorr <- pindcorr[-a$cmscoi]
      }
    }
    ccc$Calibration$Analysis$Values$Pressure <- setCcl(ccc$Calibration$Analysis$Values$Pressure,
                                                     "ind_corr",
                                                       pUnit,
                                                       pindcorr,
                                                       msg)
  }

  PNIND <- getSubList(a$cmv, "pn_ind")
  if(!is.null(PNIND)){
    PNOFF <- getSubList(a$cmv, "pn_ind_offset")

    pnpind <- getConstVal(NA,NA,PNIND) * getConvFactor(ccc,pUnit,PNIND$Unit)
    pnpoff <- getConstVal(NA,NA,PNOFF) * getConvFactor(ccc,pUnit,PNOFF$Unit)
    pnpindcorr <- pnpind - pnpoff

    if(length(a$cmscoi) > 0){
      if(a$cmscoi[1] > 0){
        pnpindcorr <- pnpindcorr[-a$cmscoi]
      }
    }
    ccc$Calibration$Analysis$Values$Pressure <- setCcl(ccc$Calibration$Analysis$Values$Pressure,
                                                     "pn_ind_corr",
                                                       pUnit,
                                                       pnpindcorr,
                                                       msg)
  }


  return(ccc)
}
