ce3.extrapC <- function(ccc){
  
  msg       <- "calculated by ce3.extrapC"
  a         <-  abbrevList(ccc)

  ## andere Gase kommen noch
  if(a$cmscg == "N2" || a$cmscg == "Ar"|| a$cmscg == "D2"){

    ilw    <- getConductIndex(ccc)
  
    cf        <- list()
    plw       <- getConstVal(a$cav, "lw")   ## zum Zeitpunkt der LW- Messung: p_l
    pfe       <- getConstVal(a$cav, "fill") ## zum Zeitpunkt Ende der Auslese: p_a
    pfill     <- (plw + pfe)/2              ## bei einem SRG ist das hier so:

    ## |--------------------------------------------->
    ##   ^tlw start         ^tlw end         ^t auslese
    ## |--------------------------------------------->
    ##              ^t lw
    ## |--------------------------------------------->
    ##                            ^t srgmv
    cnom      <- getConstVal(a$cav, "cnom")
    cfm3      <- rep(NA, length(pfill))
    dh        <- rep(NA, length(pfill))
    gas       <- a$cmscg
   
    if(length(ilw$iLw2) > 0){
      cf$a   <-  getConstVal(a$cms, paste("klLw_",gas,"_A", sep=""))
      cf$b   <-  getConstVal(a$cms, paste("klLw_",gas,"_B", sep=""))
      cf$c   <-  getConstVal(a$cms, paste("klLw_",gas,"_C", sep=""))
      cf$d   <-  getConstVal(a$cms, paste("klLw_",gas,"_D", sep=""))
      ##
      ## Verhältniss
      ## C(p_a)/C(p_l) = fn(p_a)/fn(pl)
      ## d.h.
      ## C(p_a) = C(p_l) * fn(p_a)/fn(pl)
      ##
      cfm3[ilw$iLw2]  <- cnom[ilw$iLw2] * fn.2162(cf,pfill[ilw$iLw2])/fn.2162(cf,plw[ilw$iLw2])
      ## ^^C(p_a)^^       ^^C(p_l)^^      ^^fn(p_a)^^                 ^^fn(pl)^^
      dh[ilw$iLw2]    <- cnom[ilw$iLw2]/fn.2162(cf,plw[ilw$iLw2]) - 1   
    }

    if(length(ilw$iLw1) > 0){
      cf$a  <-  getConstVal(a$cms, paste("grLw_",gas,"_A", sep=""))
      cf$b  <-  getConstVal(a$cms, paste("grLw_",gas,"_B", sep=""))
      cf$c  <-  getConstVal(a$cms, paste("grLw_",gas,"_C", sep=""))
      cf$d  <-  getConstVal(a$cms, paste("grLw_",gas,"_D", sep=""))
      ## wie oben

      cfm3[ilw$iLw1]  <- cnom[ilw$iLw1]*fn.2162(cf,pfill[ilw$iLw1])/fn.2162(cf,plw[ilw$iLw1])
      dh[ilw$iLw1]    <- cnom[ilw$iLw1]/fn.2162(cf,plw[ilw$iLw1]) - 1 
    }
  }

  ccc$Calibration$Analysis$Values$Conductance <-
    setCcl(ccc$Calibration$Analysis$Values$Conductance,
           "cfm3",
           "l/s",
           cfm3,
           msg)
  
  ccc$Calibration$Analysis$Values$Conductance <-
    setCcl(ccc$Calibration$Analysis$Values$Conductance,
           "diff_hist",
           "1",
           dh,
           paste(msg, "relative differenz to historical values (fit curve fn.2162)"))
  
  return(ccc)
}
