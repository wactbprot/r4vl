pfill   <- as.numeric(infList$args[3])
lw      <- infList$args[4]
a       <-  abbrevList(doc)

## andere Gase kommen noch
if((a$cmscg == "N2" || a$cmscg == "Ar") && is.numeric(pfill)){
  gas <- a$cmscg
  cf  <- list()
  E   <- 0.002 ## war 0.001 (mehr zeit bei 350 mbar) 
  tc  <- 2e-3  ## call offset 2ms

  
  if(lw == "lw0" ){ ## kl.LW
    cf$a   <-  getConstVal(a$cms, paste("klLw_",gas,"_A", sep=""))
    cf$b   <-  getConstVal(a$cms, paste("klLw_",gas,"_B", sep=""))
    cf$c   <-  getConstVal(a$cms, paste("klLw_",gas,"_C", sep=""))
    cf$d   <-  getConstVal(a$cms, paste("klLw_",gas,"_D", sep=""))
  

    noMp   <- 200
    V      <- 0.07673 ## l aus AA
   
  }

  if(lw == "lw1" ){ ## gr.LW
    cf$a  <-  getConstVal(a$cms, paste("grLw_",gas,"_A", sep=""))
    cf$b  <-  getConstVal(a$cms, paste("grLw_",gas,"_B", sep=""))
    cf$c  <-  getConstVal(a$cms, paste("grLw_",gas,"_C", sep=""))
    cf$d  <-  getConstVal(a$cms, paste("grLw_",gas,"_D", sep=""))
    
    noMp  <- 100
    V     <- 0.2808 ## l aus AA
  }
  
  T       <-  V/fn.2162(cf,pfill) * E ## gesamtmesszeit pro SZ
  tm      <- (T/ noMp - tc)*1000

  cat(toJSON(list("sz_time"=tm, "mp_repeat"= noMp)))

}
