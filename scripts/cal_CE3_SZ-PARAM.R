#'
#' Function calculates the CE3 saw tooth params which will be written
#' to the provided textboxes by YAMP.
#'
#' @author wactbprot (thsteinbock@web.de)
#' @export
#' @keywords yamp
#'

pcal   <- as.numeric(infList$args[3])

a       <-  abbrevList(doc)

## andere Gase kommen noch
if((a$cmscg == "N2" || a$cmscg == "Ar" || a$cmscg == "D2") & is.numeric(pcal)){
  gas <- a$cmscg
  cf  <- list()

  if(pcal < 9e-7){
      lw <- "lw0"
  }else{
      lw <- "lw1"
  }

  Cp <- getConstVal(a$cms,"nomC1") * 100 # f. l/s

  
  if(lw == "lw0" ){ ## kl.LW
    cf$a   <-  getConstVal(a$cms, paste("klLw_",gas,"_A", sep=""))
    cf$b   <-  getConstVal(a$cms, paste("klLw_",gas,"_B", sep=""))
    cf$c   <-  getConstVal(a$cms, paste("klLw_",gas,"_C", sep=""))
    cf$d   <-  getConstVal(a$cms, paste("klLw_",gas,"_D", sep=""))
    E      <- 0.00095 ## 
    noMp   <- 100
    V      <- 0.07673 ## l aus AA
    tc     <- 1.8e-3  ## call offset 2ms
    pfill  <- 0.26    ## start pfill
  }

  if(lw == "lw1" ){ ## gr.LW
    cf$a  <-  getConstVal(a$cms, paste("grLw_",gas,"_A", sep=""))
    cf$b  <-  getConstVal(a$cms, paste("grLw_",gas,"_B", sep=""))
    cf$c  <-  getConstVal(a$cms, paste("grLw_",gas,"_C", sep=""))
    cf$d  <-  getConstVal(a$cms, paste("grLw_",gas,"_D", sep=""))
    E     <- 0.0015 ## war 0.001 (mehr zeit bei 350 mbar) 
    noMp  <- 90
    V     <- 0.2808 ## l aus AA
    tc    <- 2.1e-3  ## call offset 2ms
    pfill <- 8.1    ## start pfill
  }

  pcal.pred <- fn.2162(cf,pfill)/Cp * pfill
  
  while(abs(pcal.pred/pcal - 1) > 1e-3){

      e         <- pcal.pred/pcal - 1
      pfill     <- pfill * (1+e)
      pcal.pred <- fn.2162(cf,pfill)/Cp * pfill
      print(e)
      print(pfill)
  }

  
  T       <-  V/fn.2162(cf,pfill) * E - noMp * tc ## gesamtmesszeit pro SZ
  tm      <- (T/noMp) * 1000

  cat(toJSON(list("sz_time"=tm, "mp_repeat"= noMp, lwx=lw, "target_p_fill" = pfill)))

}
