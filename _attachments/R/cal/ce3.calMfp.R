ce3.calMfp <- function(ccc){
  msg <- "calculated by calMfp"

  a <-  abbrevList(ccc)


  mbarls2pam3s  <- getConstVal(a$cc,"mbarl/s_2_Pam^3/s")
  Kb            <- getConstVal(a$cc,"Kb")
  u             <- getConstVal(a$cc,"u")       ## atom mass const in kg

  if(a$cmsc$Gas =="N2"){
    M          <- getConstVal(a$cc,"molWeight_N2") ## [M] = kg/mol
    visc       <- getConstVal(a$cc,"visc_N2")
  }
  if(a$cmsc$Gas =="Ar"){
    M          <- getConstVal(a$cc,"molWeight_Ar") ## [M] = kg/mol
    visc       <- getConstVal(a$cc,"visc_Ar")
  }
  if(a$cmsc$Gas =="D2"){
    M          <- getConstVal(a$cc,"molWeight_D2") ## [M] = kg/mol
    visc       <- getConstVal(a$cc,"visc_D2")
  }

  Mr <- M*1000 ## relative Teilchenmasse [Mr]=1

  if(a$cmscok == "opK1" | a$cmscok == "opK2" | a$cmscok == "opK4" ){
    Tch       <- getSubList(a$ca,"Tuhv")$Value ## uhv-Kammer
    nomC      <- getConstVal(a$cms,"nomC1")
  }
  if(a$cmscok == "opK3" ){
    Tch       <- getSubList(a$ca,"Txhv")$Value ## xhv-Kammer
    nomC      <- getConstVal(a$cms,"nomC2")
  }

  qpV <-  getSubList(a$ca,"qpV")$Value ## ist schon aufgeteilter Gasstrom

  p <- qpV / nomC * mbarls2pam3s ## nomC* in m^3/s, qpv* in mbar l/s
                                        #d.h Drücke hier in Pa

  ## Gleichung nach Scharipov
  ## Data on internal rarefied gas flows
  ## 1998, J. Phys. Chem Ref. Data, Vol. 27,No 3
  mfp <- sqrt(pi)* visc/(2*p) * sqrt(2* Kb * Tch/(Mr*u)) ## [l] = m

  ccc$Calibration$Analysis$Values$MeanFreePath <-
    setCcl(ccc$Calibration$Analysis$Values$MeanFreePath,"meanFreePath","m",mfp, msg)


  return(ccc)

}
