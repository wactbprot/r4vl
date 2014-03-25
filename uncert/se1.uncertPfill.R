se1.uncertPfill <- function(ccc){

  msg <- "Calculated by se1.uncertPfill()"
  a   <- abbrevList(ccc)
  
  RANGE.10     <- getSubList(a$cmco, "cdg10UseDev")
  RANGE.100    <- getSubList(a$cmco, "cdg100UseDev")
  RANGE.1000   <- getSubList(a$cmco, "cdg1000UseDev")
  PFILL        <- getSubList(a$cav, "fill")
  pfill        <- getConstVal(NA,NA,PFILL)

  upfill       <- rep(NA, length(pfill)) 
  
  if(PFILL$Unit == "mbar"){
      
      i.10           <- which((pfill > as.numeric(RANGE.10$From))   &
                              (pfill < as.numeric(RANGE.10$To)))
      i.100          <- which((pfill > as.numeric(RANGE.100$From))  &
                              (pfill < as.numeric(RANGE.100$To)))
      i.1000         <- which((pfill > as.numeric(RANGE.1000$From)) &
                              (pfill < as.numeric(RANGE.1000$To)))
      
      ## --- 10Torr CDG --- wie in QS 2/14 vorgegeben
      if(length(i.10) > 0){
          u1 <- getConstVal(a$cmco, "cdg10_u1") ## rel. 
          u2 <- getConstVal(a$cmco, "cdg10_u2")/pfill ## abs.
          u4 <- getConstVal(a$cmco, "cdg10_u4") ## rel. 
          u5 <- getConstVal(a$cmco, "cdg10_u5") ## rel. 
          u6 <- getConstVal(a$cmco, "cdg10_u6") ## rel. 
          u7 <- getConstVal(a$cmco, "cdg10_u7")/pfill ## abs.
          u8 <- getConstVal(a$cmco, "cdg10_u8")  ## rel.          

          upfill[i.10] <- sqrt(u1^2 +u2^2 +u4^2 +u5^2 +u6^2 +u7^2 + u8^2)[i.10]
      }## 10Torr
      
      ## --- 100Torr CDG ---  wie in QS 3/14 vorgegeben
      if(length(i.100) > 0){
          U1a <- getSubList(a$cmco, "cdg100_u1_a")
          U1b <- getSubList(a$cmco, "cdg100_u1_b")
          iu1a     <- which((pfill > as.numeric(U1a$From))   &
                            (pfill < as.numeric(U1a$To)))
          iu1b     <- which((pfill > as.numeric(U1b$From))   &
                            (pfill < as.numeric(U1b$To)))
          u1       <- rep(NA, length(pfill)) 
          u1[iu1a] <- getConstVal(NA, NA,  U1a)
          u1[iu1b] <- getConstVal(NA, NA,  U1b)

          u2  <- getConstVal(a$cmco, "cdg100_u2")/pfill ## abs.
          u4  <- getConstVal(a$cmco, "cdg100_u4") ## rel. 
          u5  <- getConstVal(a$cmco, "cdg100_u5") ## rel. 
          u6  <- getConstVal(a$cmco, "cdg100_u6") ## rel. 
          u7  <- getConstVal(a$cmco, "cdg100_u7")/pfill ## abs.
          u8  <- getConstVal(a$cmco, "cdg100_u8")  ## rel.          
          
          upfill[i.100] <- sqrt(u1^2 +u2^2 +u4^2 +u5^2 +u6^2 +u7^2 + u8^2)[i.100]
      }## 100Torr
      
      ## --- 1000Torr CDG --- wie in QS 4/14 vorgegeben
      if(length(i.1000) > 0){
          U1a <- getSubList(a$cmco, "cdg1000_u1_a")
          U1b <- getSubList(a$cmco, "cdg1000_u1_b")
          iu1a     <- which((pfill > as.numeric(U1a$From))   &
                            (pfill < as.numeric(U1a$To)))
          iu1b     <- which((pfill > as.numeric(U1b$From))   &
                            (pfill < as.numeric(U1b$To)))
          u1       <- rep(NA, length(pfill)) 
          u1[iu1a] <- getConstVal(NA, NA,  U1a)
          u1[iu1b] <- getConstVal(NA, NA,  U1b)
          
          u2  <- getConstVal(a$cmco, "cdg1000_u2")/pfill ## abs.
          u4  <- getConstVal(a$cmco, "cdg1000_u4") ## rel. 
          u5  <- getConstVal(a$cmco, "cdg1000_u5") ## rel. 
          u6  <- getConstVal(a$cmco, "cdg1000_u6") ## rel. 
          u7  <- getConstVal(a$cmco, "cdg1000_u7")/pfill ## abs.
          u8  <- getConstVal(a$cmco, "cdg1000_u8")  ## rel.          
          
          upfill[i.1000] <- sqrt(u1^2 +u2^2 +u4^2 +u5^2 +u6^2 +u7^2 + u8^2)[i.1000]
          
      } # 1000T
  } # mbar

  
  ccc$Calibration$Analysis$Values$Uncertainty <-
      setCcl(ccc$Calibration$Analysis$Values$Uncertainty,
             "uncertPfill",
             "1",
             upfill,
             msg)

  return (ccc)
}
