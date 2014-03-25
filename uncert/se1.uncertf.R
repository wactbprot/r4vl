se1.uncertf <- function(ccc){

  msg     <- "Calculated by se1.uncertf()"
  a       <- abbrevList(ccc)

  pfill   <- getConstVal(a$cav, "fill")
  E.name  <- unlist(getSubList(a$cmv$Expansion, "name")$Value)
  uf      <- rep(NA, length(pfill))
  
  i.a <- which(E.name == "Expansion_A")
  i.b <- which(E.name == "Expansion_B")
  i.c <- which(E.name == "Expansion_C")
  i.d <- which(E.name == "Expansion_D")
  i.e <- which(E.name == "Expansion_E")

  if(length(i.a) > 0){
      uf[i.a] <- getConstVal(a$cms, "fA_u1")
  }
  if(length(i.b) > 0){
      uf[i.b] <- getConstVal(a$cms, "fB_u1")
  }
  if(length(i.c) > 0){
      uf[i.c] <- getConstVal(a$cms, "fC_u1")
  }
  if(length(i.d) > 0){
      uf[i.d] <- getConstVal(a$cms, "fD_u1")
  }
  if(length(i.e) > 0){
      uf[i.e] <- getConstVal(a$cms, "fE_u1")
  }

  ccc$Calibration$Analysis$Values$Uncertainty <-
      setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
             "uncertf",
             "1",
             uf,
             msg)

  return(ccc)
}
