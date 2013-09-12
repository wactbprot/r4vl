#'
#' Function calculates the SE1 Expansion and filling pressure.
#'
#' @author wactbprot (thsteinbock@web.de)
#' @export
#' @keywords yamp
#'

## '/usr/local/lib/r4vl/cal.R',
##      'scripts/cal_SE1_P-PARAM.R',
##      '0.000002',
##      'mbar',
##      'localhost',
##      'vaclab_db',
##      'e313d0a66ef1b6d24dd0d3753d030946' ],

targetpcal   <- as.numeric(infList$args[2])
unit         <- infList$args[3]


if(length(doc$Calibration) > 0 &
   doc$Calibration$Standard == "SE1" &
   is.numeric(targetpcal)){

  toUnit <- "mbar"
  conv   <- getConvFactor(doc,toUnit, unit)
  pcal   <- targetpcal * conv
  a      <-  abbrevList(doc)
  exName      <- c(
                   "Expansion_A",
                   "Expansion_B",
                   "Expansion_E")

  N        <- length(exName)
  p        <- rep(NA,N)
  startVol <- rep(NA,N)
  funcorr  <- rep(NA,N)

  for( i in 1:N){

    fStruct     <- getSubList(a$cms,exName[i])

    startVol[i] <- as.numeric(getConstVal(a$cms,fStruct$StartVolume))
    funcorr[i]  <- as.numeric(fStruct$Value)

    p[i]        <- targetpcal/funcorr[i]
  }

  iOk <- which( p > 1 & p < 1000)
  
  if(length(iOk) > 0){
    pSel      <- p[iOk]
    exNameSel <- exName[iOk]
    fSel      <- funcorr[iOk]
    VSel      <- startVol[iOk]
    ## der größte Fülldruck hat die
    ## geringste Unsicherheit
    iSel      <- which.max(pSel)



    cat(toJSON(list("expansion"   = exNameSel[iSel],
                    "p_fill_mbar" = pSel[iSel],
                    "f_uncorr"    = fSel[iSel],
                    "start_vol"   = VSel[iSel]
                    )))
  }else{

    cat(toJSON(list("expansion"   = "keinen",
                    "p_fill_mbar" = "passenden",
                    "f_uncorr"    = "gefunden",
                    "start_vol"   = "~")))

  }

}else{
  cat(toJSON(list("expansion"   = "~",
                  "p_fill_mbar" = "~",
                  "f_uncorr"    = "~",
                  "start_vol"   = "~")))
}
