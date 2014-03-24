cuco.uncertDigit <- function(ccc){
  msg <- "calculated by cuco.uncertDigit"

  a   <- abbrevList(ccc)

  un <- "digit"
  
  PIND  <- getSubList(a$cav, "ind")
  pind  <- getConstVal(NA,NA,PIND)
  
  digit  <- getConstVal(a$cmco1, un)
  
  if(length(digit) == 0){
      digit <- 0.02
  }

  fp <- formatC(pind,format="E")
  
  m <- regexec("E[+-][0-9]*$",fp)
  u <- as.numeric(paste(digit,unlist(regmatches(fp, m)), sep=""))*0.29 

  ccc$Calibration$Analysis$Values$Uncertainty <-
      setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
             "uncertDigit",
             "1",
             u/pind,
             msg)
  
  return(ccc)
}
