ce3.calT <- function(ccc){

  msg <- "calculated by ce3.calT"
  
  a <- abbrevList(ccc)
  
  C2K <- getConstVal(a$cc, "C_2_K")
  
  ## ab heute(11.10.11) wird nur noch das agilent
  ## zur emperaturberechnung benutzt
  ## weil T_ptb - Tch als Korrektur benutzt wird, muss
  ## Tch + K gerechnet werden: Tch + K =   Tch + (T_ptb - Tch) = T_ptb
 
  
  tRoom <-  (getConstVal(a$cmv,"meas-agilentCh110") +
             getConstVal(a$cmco,"agilentCorrCh110"))
  
  tpbox <-  (getConstVal(a$cmv,"meas-agilentCh103") +
             getConstVal(a$cmco,"agilentCorrCh103"))
  
  tUhv <- ((getConstVal(a$cmv,"meas-agilentCh104") +
            getConstVal(a$cmco,"agilentCorrCh104")) +
           (getConstVal(a$cmv,"meas-agilentCh105") +
            getConstVal(a$cmco,"agilentCorrCh105")) +
           (getConstVal(a$cmv,"meas-agilentCh106") +
            getConstVal(a$cmco,"agilentCorrCh106")) +
           (getConstVal(a$cmv,"meas-agilentCh107") +
            getConstVal(a$cmco,"agilentCorrCh107"))) /4
  
  tXhv <- ((getConstVal(a$cmv,"meas-agilentCh108") +
            getConstVal(a$cmco,"agilentCorrCh108")) +
           (getConstVal(a$cmv,"meas-agilentCh109") +
             getConstVal(a$cmco,"agilentCorrCh109"))) /2
  
  tFm <- ((getConstVal(a$cmv,"lw-agilentCh101") +
           getConstVal(a$cmco,"agilentCorrCh101")) +
          (getConstVal(a$cmv,"lw-agilentCh102") +
           getConstVal(a$cmco,"agilentCorrCh102"))) /2
  
  
  
  ## mit RJSONIO kann alles rund um outIndex verworfen werden
  ## das muss aber in allen cal*.R files gleichzeitig passieren
  
  if(length(a$cmscoi) > 0){
    if(a$cmscoi[1] > 0){
      
      tRoom <- tRoom[-a$cmscoi]
      tUhv<-tUhv[-a$cmscoi]
      tXhv<-tXhv[-a$cmscoi]
      tFm<-tFm[-a$cmscoi]
 
    }
  }
 
  if(length(ccc$Calibration$Analysis$Values) == 0 ){
    ccc$Calibration$Analysis$Values <- list()
  }
 
  ccc$Calibration$Analysis$Values$Temperature <-
    setCcl( ccc$Calibration$Analysis$Values$Temperature,"Tfm3","K",tFm + C2K,msg)
  ccc$Calibration$Analysis$Values$Temperature <-
    setCcl( ccc$Calibration$Analysis$Values$Temperature,"Tuhv","K",tUhv + C2K,msg)
  ccc$Calibration$Analysis$Values$Temperature <-
    setCcl( ccc$Calibration$Analysis$Values$Temperature,"Txhv","K",tXhv + C2K,msg)
  ccc$Calibration$Analysis$Values$Temperature <-
    setCcl( ccc$Calibration$Analysis$Values$Temperature,"Troom","K",tRoom + C2K,msg)
  ccc$Calibration$Analysis$Values$Temperature <-
    setCcl( ccc$Calibration$Analysis$Values$Temperature,"Tpbox","K",tpbox + C2K,msg)
 
  return(ccc)

}
