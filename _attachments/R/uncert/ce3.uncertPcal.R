ce3.uncertPcal <- function(ccc){

  msg  <- "Calculated by ce3.uncertPcal()"
  a    <- abbrevList(ccc)

  PCAL  <- getSubList(a$cav,  "cal"    )

  UQPV  <- getSubList(a$cav,  "uncertqpV"    )
  UQsp  <- getSubList(a$cav,  "uncertQsplit" )
  UF    <- getSubList(a$cav,  "uncertF"      )
  UTch  <- getSubList(a$cav,  "uncertTch"    )
  UTfm  <- getSubList(a$cav,  "uncertTfm"    )
  UCx   <- getSubList(a$cav,  "uncertCx"     )
  

  if(UQPV$Unit      == "1" &&
     UQsp$Unit      == "1" &&
     UF$Unit        == "1" &&
     UTch$Unit      == "1" &&
     UTfm$Unit      == "1" &&
     UCx$Unit       == "1"   ){

    pcal  <- getConstVal( NA,NA,  PCAL )
    
    uqpv  <- getConstVal( NA,NA,  UQPV )
    uqsp  <- getConstVal( NA,NA,  UQsp )
    uf    <- getConstVal( NA,NA,  UF   )
    uTch  <- getConstVal( NA,NA,  UTch )
    uTfm  <- getConstVal( NA,NA,  UTfm )
    uCx   <- getConstVal( NA,NA,  UCx  )
    
    uncertges <- sqrt( uqpv^2 + uqsp^2 + uf^2 + uTch^2 + uTfm^2 + uCx^2)
    
  }
  
  ccc$Calibration$Analysis$Values$Uncertainty <-
    setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
           "uncertPcal_rel",
           "1",
           uncertges,
           paste(msg, " (k=1)"))

  ccc$Calibration$Analysis$Values$Uncertainty <-
    setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
           "uncertPcal_abs",
           PCAL$Unit,
           uncertges * pcal,
           paste(msg, " (k=1)"))
  
  
    return(ccc)
}
