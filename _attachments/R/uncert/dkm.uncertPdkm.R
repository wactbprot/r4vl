dkm.uncertPdkm <- function(ccc){

  msg <- "Calculated by dkm.uncertPdkm()"
  a <- abbrevList(ccc)

  calUnit   <- "Pa"

  PDKM      <- getSubList(a$cav$Pressure,    "dkm")
  convpdkm   <- getConvFactor(ccc,calUnit,PDKM$Unit)
  pdkm        <- getConstVal(NA,NA,PDKM)    * convpdkm  # ab hier Pa wenn nicht sowieso

  PDKMRES      <- getSubList(a$cav$Pressure,    "dkm_res")
  convpdkmres   <- getConvFactor(ccc,calUnit,PDKMRES$Unit)
  pdkmres        <- getConstVal(NA,NA,PDKMRES)    * convpdkmres  # ab hier Pa wenn nicht sowieso


  SYSTCHAN  <-  getSubList(a$cms,"system_change")
  convSC    <- getConvFactor(ccc,calUnit,SYSTCHAN$Unit)
  systChange  <- getConstVal(NA,NA,SYSTCHAN)* convSC   # Pa

  udkm <- rep(0,length(pdkm))

  ## hier die Unsicherheiten aus DB:
  ##  (ohne check der Einheit)


  u1s1      <- getConstVal(a$cms, "u_dkm_1_system_1") ## druckabh. Teil
  u1s2      <- getConstVal(a$cms, "u_dkm_1_system_2")

  u2s1      <- getConstVal(a$cms, "u_dkm_2_system_1") ## druck_un_abh. Teil
  u2s2      <- getConstVal(a$cms, "u_dkm_2_system_2")

  u3        <- getConstVal(a$cms, "u_dkm_3")
  upres     <- getConstVal(a$cmco, "u_dkm_res")


  ilo <- which(pdkm < systChange)
  ihi <- which(pdkm >= systChange)

  ## es ist hier wieder keine Kontrolle des outIndex nötig,
  ## da alle vektoren aus aus Analysis kommen (also schon über
  ## den outIndex gelaufen sind

  if(length(ilo) > 0){
    udkm[ilo] <- sqrt(u1s1^2 + (u2s1*pdkm[ilo])^2 + (u3*pdkm[ilo])^2 +  (upres*pdkmres[ilo])^2)

  }
  if(length(ihi) > 0){
    udkm[ihi] <- sqrt(u1s2^2 + (u2s2*pdkm[ihi])^2 + (u3*pdkm[ihi])^2 +  (upres*pdkmres[ihi])^2)

  }

  ccc$Calibration$Analysis$Values$Uncertainty <-
    setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
           "u_dkm",
           calUnit,
           udkm,
           msg)

  return(ccc)
}
