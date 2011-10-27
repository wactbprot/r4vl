ce3.uncertDeltaV <- function(ccc){

  msg <- "Calculated by ce3.uncertDeltaV()"

  
  a <- abbrevList(ccc)

  
  PFILL <- getSubList(a$ca, "fill")
  
  pfill     <- getConstVal(NA,NA,PFILL)
  pfillUnit <- PFILL$Unit
  noOfPfill <- length(pfill)

  ##
  ## später evtl noch range def.
  ##

  uncertDeltaV <- rep(0,noOfPfill)

  
  u2aList <-  getSubList(a$cms,"fm3DeltaV_u2_a")
  u2aAbs <- getConstVal(NA,NA,u2aList)

  u2cList <-  getSubList(a$cms,"fm3DeltaV_u2_c")
  u2cAbs <- getConstVal(NA,NA,u2cList)

  deltaGList <-  getSubList(a$cms,"deltaG")
  deltaG <- getConstVal(NA,NA,deltaGList)

  if(u2aList$Unit ==  deltaGList$Unit){
    u2a <-  u2aAbs/deltaG
  }else{
    print("unit deltaG and fm3DeltaV_u2_a don't match")
    stop()
  }
  if(u2cList$Unit ==  deltaGList$Unit){
    u2c <-  u2cAbs/deltaG
  }else{
    print("unit deltaG and fm3DeltaV_u2_c don't match")
    stop()
  }

  u2bList <- getSubList(a$cms,"fm3DeltaV_u2_b")
  u2b <-  getConstVal(NA,NA,u2bList)
  iu2b <- checkUncertRange(u2bList, PFILL)

  u2dList <- getSubList(a$cms,"fm3DeltaV_u2_d")
  u2d <-  getConstVal(NA,NA,u2dList)
  iu2d <- checkUncertRange(u2dList, PFILL)

  u2eList <- getSubList(a$cms,"fm3DeltaV_u2_e")
  u2e <-  getConstVal(NA,NA,u2eList)
  iu2e <- checkUncertRange(u2eList, PFILL)

  u2fList <- getSubList(a$cms,"fm3DeltaV_u2_f")
  u2f <-  getConstVal(NA,NA,u2fList)
  iu2f <- checkUncertRange(u2fList, PFILL)

  u2gList <- getSubList(a$cms,"fm3DeltaV_u2_g")
  u2g <-  getConstVal(NA,NA,u2gList)
  iu2g <- checkUncertRange(u2gList, PFILL)
  
  if((length(iu2d) == length(iu2e)) &
     (length(iu2e) == length(iu2f)) &
     (length(iu2f) == length(iu2g))){
    ## Gleichung 16, 17 und 18:
    uDeltaG <- sqrt(u2a^2 + u2b^2 + u2c^2)
    ## Gleichung 15
    uA      <- sqrt(uDeltaG^2 + u2d^2 + u2e^2 + u2g^2)
    ## Gleichung 13
    uncertDeltaV[iu2g] <- sqrt(uA^2 + u2e^2 + u2f^2)
    
    msg <- paste(msg,"relativ Uncertainty is related to DeltaV!")
    
    ccc$Calibration$Analysis$Values$Uncertainty <-
      setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
             "uncertDeltaV",
             "1",
             uncertDeltaV,
             msg)
  }else{
    stop()
  }
  return(ccc)
}
## c    Rel. Varianz von Delta V
##      Y2=S3(p0(I))+S6(p0(I))+S7(p0(I))  !für Publikation 3 bis 5 zusammen in 3
## c       Y2=S3(p0(I))+S4(p0(I))+S5(p0(I))+S6(p0(I))+S7(p0(I))
##
### ...
## C    Rel Unsicherheit von q_pV
##      Y(I)=Y1+Y2+Y3
## c    if(p0(I).lt.10) Y(I)=Y1+Y2+Y3+S10(10.,ILeit)+S11(p0(I),ILeit)
## c    Y(I)=Y1+Y2+Y3+S11(p0(I),Ileit)                !int.Vergl.
## c    if(FANT.ne.1) Y(I)=Y(I)+S13(p0(I))/296.15**2
##      if(FANT.eq.3) q(I)=q(I)/(8314*296.15)       !in mol/s
## c
##      Y(I)=sqrt(Y(I))
##      Y(I)=float(k)*Y(I)
### ...
##      REAL FUNCTION S3(p0) !Verdunstung Gl.(17)
## C    Für Publikation summarische Unsicherheit von S3 bis S5
## C    S3=5.e-5/.5             !relativ fm3
##      S3=3.1e-4               !summarisch fuer A_eff
##      S3=S3*S3
##      END
## C
##      REAL FUNCTION S4(p0) !Gewichtsmessung Wasser in Verdr.
##      S4=(4.e-5/.5)**2+9.e-8     !Gl.(18) durch Delta G
##      END
## C
##      REAL FUNCTION S5(p0) !Unsicherheit Dichte
##      S5=2.5e-5              !Gl. (19)
##      S5=S5*S5
##      END
## c
##      REAL FUNCTION S6(p0) !Delta L Gl.(21)
## C    S6=(1.e-6/3.e-3)**2/12.          !Gl.(21) relativ
##      S6=(1.4e-4)**2                  !fuer Publikation
##      END
## C
##      REAL FUNCTION S7(p0) !Diff. auss-innen Verdr. Gl. (22)
##      S7=1.e-8             !27.6.01 fuer Publ.
##      END
## C
