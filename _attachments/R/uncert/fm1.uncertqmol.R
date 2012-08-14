fm1.uncertqmol <- function(ccc){

  msg <- "Calculated by fm1.uncertqmol()"
  a   <- abbrevList(ccc)

  
  UQPV       <-  getSubList(a$cav, "uncertqpV_rel")
  uqpV       <-  getConstVal(NA,NA,UQPV)

  uTfm      <-  getConstVal(a$cav, "uncertTfm")
  
  ## 
  ## QPV         <-  getSubList(a$cav, "qpV")
  ## qpV         <-  getConstVal(NA,NA,QPV)
  ## 
  ## R           <-  getConstVal(a$cc,"R")
  ## Tfm         <-  getConstVal(a$cav,"fm1")
  ## 
  ## Pam3molK2mbarlmolK <- getConstVal(a$cc, "Pam^3/mol/K_2_mbarl/mol/K")
  ## R                  <- getConstVal(a$cc,"R") * Pam3molK2mbarlmolK ## in Pa m^3/mol/K
  ## 
  ## 
  ## qmol        <-  qpV/(R*T)
  

  uqmol <- sqrt(uqpV^2 + uTfm ^2)
  
  ccc$Calibration$Analysis$Values$Uncertainty <-
    setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
           "uncertqmol_rel",
           "1",
           uqmol,
           msg)
  
  return(ccc)
}
