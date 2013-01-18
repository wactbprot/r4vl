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
  f      <- c(
           "Expansion_A",
           "Expansion_B",
           "Expansion_C",
           "Expansion_D",
           "Expansion_E")
  
  N <- length(f)

  p <- rep(NA,N) 
  
  for( i in 1:N){
    p[i] <- targetpcal/as.numeric(getSubList(a$cms,f[i])$Value)
  }
  
  i1 <- which( p > 10 & p < 1000)
  if(length(i1) > 0){
    
    p1 <- p[i1]
    f1 <- f[i1]
    i2 <- which.max(p1)
    
  }
    
  cat(toJSON(list("expansion"=f[i2], "p_fill_mbar"= p[i2], "p_fill_V"= p[i2]*10/1000)))
}else{
  cat(toJSON(list("expansion"="~", "p_fill_mbar"= "~", "p_fill_V"= "~")))
}
