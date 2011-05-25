calPfill <- function( ccc){
### muss noch sinnvoll mit calpfill der VACOM Kalib gemerged werden !!
  msg <- "calculated by calPfill"

  a <- abbrevList(ccc)


  if(a$cmscok == "opK1" |a$cmscok == "opK2"|a$cmscok == "opK3"){
    ## gefährliche Stelle wenn mehr als ein Measurement
    ## existiert
    ## geand. von Measurement$Values -> Measurement entschärft die Sache

    ## soll hier immer gerechnet werden
    ##if( length(getSubList(a$ca, "fill")$Value) == 0){

    PFIL <- getSubList(a$cmv, "fill")

    pfill <- getConstVal(NA, NA, PFIL)

    

    ##if(length(a$cmscoi) > 0){
    ##  if(a$cmscoi[1] > 0){
    ##    pfill <- pfill[-a$cmscoi]
    ##  }
    ##}durch
    pfill <- checkOutIndex(a,pfill)
    ## ersetzt

    ccc$Calibration$Analysis$Values<-  checkSetList(ccc$Calibration$Analysis$Values)
    
    if(length(PFIL$Comment) > 0){
      msg <- paste(msg,"msg from raw data:",  PFIL$Comment)
    }

    ccc$Calibration$Analysis$Values$Pressure <-
      setCcl(ccc$Calibration$Analysis$Values$Pressure,"fill",
             PFIL$Unit,
             pfill,
             msg)
    ##}
  }## OPK 1-3

  if(a$cmscok == "opK4"){
    ## opk4: sehr kleine Fülldrücke die erst noch
    ## berechnet/korrigiert werden müssen
    ##
    ## wert ist in Standard$SequenzControl zentral def.
    ## cdg.srg  <- 0.012 ## mbar für p > cdg.srg wird cdg für pfill benutzt
    ## --> anpassen!!
    noOfViscIter <- 5 ## Anzahl der Iterationen zu visc. Korrektur
    srgInd   <- getSubList(a$cm, "srgFill")
    srgOff   <- getSubList(a$cm, "srgFillOffset")

    cdgInd   <-   getSubList(a$cm, "cdgInd")
    cdgOff   <-   getSubList(a$cm, "cdgIndOffset")

    ## hier können noch viele andere Gegebenheiten im
    ## Zusammenhang mit dem p_fill implementiert werden
    ## 1.)
    if(length(srgInd$Value) > 0 & length(srgInd$Value) ==length(srgOff$Value)){

      if(srgInd$Unit == "dcr"){
        ## raw srg measurement
### Todo: was ist wenn ein sgr kalibriert wird
### und ein sgr zur pfill Messung benutzt wird!!
### Es muss ein 
        d            <- getConstVal(a$cmco,"d")
        rho          <- getConstVal(a$cmco,"rho")
        R            <- getConstVal(a$cc,"R")
        pfill        <- srgInd$Value - srgOff$Value # in DCR

        if(a$cmscg =="N2"){
          sigma      <- getConstVal(a$cmco,"sigma_N2")
          slope      <- getConstVal(a$cmco,"slope_N2")
          M          <- getConstVal(a$cc,"molWeight_N2")
          visc       <- getConstVal(a$cc,"visc_N2")
        }
        if(a$cmscg =="Ar"){
          M           <- getConstVal(a$cc,"molWeight_Ar")
          visc        <- getConstVal(a$cc,"visc_Ar")
          sigma       <- getConstVal(a$cmco,"sigma_Ar")
          slope       <- getConstVal(a$cmco,"slope_Ar")
        }
        ## N.B. T kommt aus Analysis und ist deshalb schon über a$cmscoi gelaufen
        T  <- getConstVal(a$ca,"Tuhv")

        if(length(a$cmscoi) > 0){
          if(a$cmscoi[1] > 0){
            pfill <- pfill[-a$cmscoi]
          }
        }

        pfill <- sqrt(8*R*(T)/(pi*M))*pi*d*rho/sigma/2000*pfill ## mbar 

        ## visc. correction
        ## s. http://a73434.berlin.ptb.de/vacLab/index.php5/
        ##            Messungen_Februar_09#Druckmessung_mit_SRG
        ##
        
        panz      <- pfill
        pfillKorr <- panz

        for(i in 1:noOfViscIter){

          pfillKorr <- panz*sigma/(slope*pfillKorr+sigma)

        }
        pfillSRG <- pfillKorr
        msg <- paste(msg,
                     " from dcr values with ",
                     noOfViscIter,
                     " iterations for viscosity correction" )
      }## Unit dcr
    }
    ## 2.)
    ## cdga
    if(length(cdgInd$Value) > 0 & length(cdgInd$Value) ==length(cdgOff$Value)){

      if(a$cmscg =="N2"){
        A <- getConstVal(a$cmco,"cdgaCorrA_N2")
        B <- getConstVal(a$cmco,"cdgaCorrB_N2")
        C <- getConstVal(a$cmco,"cdgaCorrC_N2")
        D <- getConstVal(a$cmco,"cdgaCorrD_N2")
        E <- getConstVal(a$cmco,"cdgaCorrE_N2")
        F <- getConstVal(a$cmco,"cdgaCorrF_N2")
      }

      pfillCDGUnkorr <- cdgInd$Value - cdgOff$Value

      if(length(a$cmscoi) > 0){
        if(a$cmscoi[1] > 0){
          pfillCDGUnkorr <- pfillCDGUnkorr[-a$cmscoi]
        }
      }

      ## F(relativ)=(a+c*pind+e*pind^2)/(1+b*pind+d*pind^2+f*pind^3)
      relErr <-  (A+C*pfillCDGUnkorr+E*pfillCDGUnkorr^2)/
        (1+B*pfillCDGUnkorr+D*pfillCDGUnkorr^2+F*pfillCDGUnkorr^3)
                                        # in mbar
      pfillCDG <- pfillCDGUnkorr/(relErr +1)
    }

    isrg <- which(pfillSRG <= cdg.srg) # mbar
    icdg <- which(pfillSRG > cdg.srg) # mbar

    ## plot(pfillCDG, (pfillSRG/pfillCDG-1)*100 )

    pfill <- c(pfillSRG[isrg],pfillCDG[icdg])

    ccc$Calibration$Analysis$Values$Pressure <- setCcl(ccc$Calibration$Analysis$Values$Pressure,
                                                       "fill",
                                                       "mbar",
                                                       pfill,
                                                       msg)
  }## opk4


  
  return( ccc )
}



