#'
#' Funktion zur Berechnung des Fülldrucks und der Sägezahnparameter 
#' 
#' @author wactbprot (thsteinbock@web.de)
#' @export
#' @keywords yamp
#'

pcal    <- as.numeric(infList$args[2])
a       <-  abbrevList(doc)

## andere Gase kommen noch
if((a$cmscg == "N2" || a$cmscg == "Ar" || a$cmscg == "D2") & is.numeric(pcal)){

    gas   <- a$cmscg
    cf    <- list()

    if(pcal < 9e-7){
        lw <- "lw0"
    }else{
        lw <- "lw1"
    }

    NOMC1 <- getSubList(a$cms,"nomC1")
    conv <- getConvFactor(doc, "l/s" , NOMC1$Unit)
                                        #^^to , ^^from
    Cp <- getConstVal(NA, NA, NOMC1) * conv # f. l/s


    if(lw == "lw0" ){
        ## Parameter
        ## kleiner Leitwert
        ##
        cf$a   <-  getConstVal(a$cms, paste("klLw_",gas,"_A", sep=""))
        cf$b   <-  getConstVal(a$cms, paste("klLw_",gas,"_B", sep=""))
        cf$c   <-  getConstVal(a$cms, paste("klLw_",gas,"_C", sep=""))
        cf$d   <-  getConstVal(a$cms, paste("klLw_",gas,"_D", sep=""))
        E      <- 0.00095 ##
        noMp   <- 100     ## Anzahl der Messpunkte
        V      <- 0.07673 ## Volumen in l aus AA
        tc     <- 1.8e-3  ## call offset 2ms
        pfill  <- 0.26    ## start pfill
    }

    if(lw == "lw1" ){
        ## Parameter 
        ## großer Leitwert
        ##
        
        cf$a  <-  getConstVal(a$cms, paste("grLw_",gas,"_A", sep=""))
        cf$b  <-  getConstVal(a$cms, paste("grLw_",gas,"_B", sep=""))
        cf$c  <-  getConstVal(a$cms, paste("grLw_",gas,"_C", sep=""))
        cf$d  <-  getConstVal(a$cms, paste("grLw_",gas,"_D", sep=""))
        E     <- 0.0015 ## 
        noMp  <- 90     ## Anzahl der Messpunkte
        V     <- 0.2808 ## Volumen in l aus AA
        tc    <- 2.1e-3 ## call-offset 2ms
        pfill <- 8.1    ## start pfill
    }
    ##
    ## Berechnen des pfill aus der Leitwertkurve
    ## und der Differenz zum Zieldruck
    ##
    pcal.pred     <- fn.2162(cf, pfill)/Cp * pfill
    for(i in 1:10){
        e         <- pcal.pred/pcal - 1
        pfill     <- pfill * (1 - e)
        pcal.pred <- fn.2162(cf, pfill)/Cp * pfill

    }

    T       <-  V / fn.2162(cf, pfill) * E - noMp * tc ## Gesamtmesszeit pro SZ
    tm      <- (T / noMp) * 1000

    cat(toJSON(list("target_p_fill" = pfill, "lwx" = lw,"sz_time"=tm, "mp_repeat"= noMp  )))

}
