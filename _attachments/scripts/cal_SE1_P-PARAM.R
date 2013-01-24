#'
#' Function calculates the SE1 Expansion and filling pressure.
#'
#' @author wactbprot (thsteinbock@web.de)
#' @export
#' @keywords yamp
#'

targetpcal   <- as.numeric(infList$args[3])
unit         <- infList$args[4]

if(is.numeric(targetpcal)){

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

    startVol[i] <- getConstVal(a$cms,fStruct$StartVol)
    funcorr[i]  <- as.numeric(fStruct$Value)
    p[i]        <- targetpcal/funcorr
  }
  
  iOk <- which( p > 10 & p < 1000)
  if(length(iOk) > 0){
    
    pSel      <- p[iOk]
    exNameSel <- exName[iOk]
    fSel      <- funcorr[iOk]
    VSel      <- startVol[iOk]
    iSel      <- which.max(pSel)
    
  }

  cat(toJSON(list("expansion"   = exNameSel[iSel],
                  "p_fill_mbar" = pSel[iSel],
                  "p_fill_V"    = pSel[iSel]*10/1000,
                  "f_uncorr"    = fSel[iSel],
                  "start_vol"   = VSel[iSel]
                  )))
}else{
  cat(toJSON(list("expansion"   = "~",
                  "p_fill_mbar" = "~",
                  "p_fill_V"    = "~",
                  "f_uncorr"    = "~",
                  "start_vol"   = "~")))
}
