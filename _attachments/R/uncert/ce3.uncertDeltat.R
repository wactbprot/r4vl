ce3.uncertDeltat <- function(ccc){

  msg <- "Calculated by ce3.uncertDeltat()"

  tmpAn     <- ccc$Calibration$Analysis
  tmpMea    <- ccc$Calibration$Measurement
  tmpStrd   <- tmpMea$Standard
  tmpCo     <- tmpMea$CalibrationObject

  pfillList <- getSubList(tmpAn, "fill")
  pfill     <- pfillList$Value
  pfillUnit <- pfillList$Unit

  pcalList <- getSubList(tmpAn, "cal")
  pcal     <- pcalList$Value

  uncertRes <- rep(1, length(pfill))

  ## welcher Lw benutzt wird, wird in getConductIndex() entschieden
  ## die Grenzen sind derzeit im Standard doc def.
  iLw1 <-  getConductIndex(ccc)$iLw1
  iLw2 <-  getConductIndex(ccc)$iLw2
 ## gibt es für jeden pfill einen Leitwert
  ## gesamte Range
    ## Lw1:
    if(length(iLw1) > 0){
      u1 <-  as.double(getSubList(tmpStrd$Values,"fm3Deltat_u1")$Value) ##zufall
      u2List <- getSubList(tmpStrd$Values,"fm3Deltat_u2")
      iu2 <- checkUncertRange(u2List, pfillList, iLw1)

      if(length(iu2) > 0){
       u2 <- as.double(u2List$Value)
       uncertRes[iu2] <- sqrt(u1^2+u2^2)

       msg <- paste(msg, "points: ",toString(iu2)," use: ",u2List$Type, "(LW1)")
     }

      u3aList <- getSubList(tmpStrd$Values,"fm3DeltatLw1_u3_a")
      iu3a <- checkUncertRange(u3aList, pfillList, iLw1)

      u3bList <- getSubList(tmpStrd$Values,"fm3DeltatLw1_u3_b")
      iu3b <- checkUncertRange(u3bList, pfillList, iLw1)
      ## da die beiden uns zusammenhängen,
      ## müssen die letzten bearbeitungspunkte auch die selben sein
      ## test kann auch wegfallen
      if((pfill[iu3a[length(iu3a)]] == pfill[iu3b[length(iu3b)]]) & (length(iu3a) > 0)){
        if(!(pfillUnit =="mbar")){
          print(paste( pfillUnit," (Unit of pfill) dont match with", u3Lbist$Type))
          stop()
        }
        u3a <- as.double(u3aList$Value)
        u3b <- as.double(u3bList$Value)

        uncertRes[iu3a] <- sqrt((u3a + log(pfill[iu3a]/1.0)*u3b)^2 +  u1^2)

        msg <- paste(msg, "points: ",toString(iu3a), " use: ",u3aList$Type, "and ",u3bList$Type, "(LW1)" )
      }
    }##LW1

    if(length(iLw2) > 0){
      u1 <-  as.double(getSubList(tmpStrd$Values,"fm3Deltat_u1")$Value) ##zufall

      u3List <- getSubList(tmpStrd$Values,"fm3DeltatLw2_u3")
      iu3 <- checkUncertRange(u3List, pfillList, iLw2)

      if(length(iu3) > 0){
        u3 <- as.double(u3List$Value)
        uncertRes[iu3] <- sqrt(u1^2+u3^2)
        msg <- paste(msg, "points: ",toString(iu3a), " use: ",u3List$Type, "(LW2)")
      }


    }## gesamte Range

  ccc$Calibration$Analysis$Values$Uncertainty <- setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
                                                        "uncertDeltat",
                                                        "1",
                                                        uncertRes,
                                                        msg)
  return(ccc)
}
##      REAL FUNCTION S8(p0,ILeit) !Drift plus Leitwert zufall !neu für Publ.
## c    S8=0.           !fuer Int. Vergl.
## c    S8=5.e-4*+ALOG10(1.e5/p0)*1.e-4   !Gl. (26), bzw. (28) relativ
##      S8=1.3e-3
##      IF(ILeit.eq.2) S8=1.3e-3+1e-3*ALOG10(200/p0)
##      S8=S8*S8
##      END
## C
##      REAL FUNCTION S9(p0,ILeit)  !Drift system.
##      S9=4.4e-5                       !fuer Publikation
## c    S9=6.e-4
## c    if(p0.ge.100.) S9=6.e-4+ALOG10(p0/100.)*2.e-4
## c    if(ILeit.eq.2) S9=4.e-3
##      if(ILeit.eq.2) S9=7.9e-4      !fuer Publikation
##      S9=S9*S9
##      END
## c
##      REAL FUNCTION S10(p0,ILeit)    !random Leitwertmessung bei 10 Pa
## c    S10=1.4e-3                      !bis 13.6.01
## c    if(p0.le.5.e3) S10=1.3e-3       !bis 13.6.01
## c    if(p0.le.50.and.ILeit.eq.1) S10=1.3e-3+2.7e-3*ALOG10(50./p0) !Bis 13.6.01
## c    if(ILeit.eq.2) S10=1.3e-3+ALOG10(100./p0)*1.2e-3  !p0<100Pa !bis 13.6.01
##      if (Ileit.eq.1) S10=3.5e-3
##      if (Ileit.eq.2) S10=2.7e-3   !Publik.
##      S10=S10*S10
##      end
