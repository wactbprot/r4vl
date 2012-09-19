ce3.extrapC <- function(ccc){
  msg <- "calculated by ce3.extrapC"

  a     <-  abbrevList(ccc)
  
  ##      return(list(iLw1 = iLw1,   ## grosser LW
  ##                  iLw2 = iLw2))  ## kleiner LW
  ##  
  ilw   <-  getConductIndex(ccc)

  ## f geht viel kürzer mit poly()
  ## so sieht man aber besser was passiert:
  f <- function(cf, x){
    r <- cf$a0 + cf$a1*x + cf$a2*x^2 + cf$a3*x^3 +cf$a4*x^4 +cf$a5*x^5 
    return(r)
  }

  
  if(a$cmscg == "N2"){
    if(length(ilw$iLw2) > 0){
      ##
      ## N2 kleiner LW
      ##
    }
    if(length(ilw$iLw1) > 0){
      ##
      ## N2 großer LW
      ##
    }

  }

  if(a$cmscg == "Ar"){
    if(length(ilw$iLw2) > 0){
      ##
      ## Ar kleiner LW
      ##
    }
    if(length(ilw$iLw1) > 0){
      ##
      ## Ar großer LW
      ##
    }

  }

  
  
  ccc$Calibration$Analysis$Values$Conductance <-
    setCcl(ccc$Calibration$Analysis$Values$Conductance,
           "cfm3",
           "l/s",
           L,
           msg)
  
  return(ccc)
}
