aim        <- 296.15
upperLimit <- 297.15
lowerLimit <- 295.15
withU      <- FALSE

Tpbox <- getConstVal(a$cav$Temperature, "Tpbox")
Troom <- getConstVal(a$cav$Temperature, "Troom")
Txhv  <- getConstVal(a$cav$Temperature, "Txhv")
Tuhv  <- getConstVal(a$cav$Temperature, "Tuhv")
Tfm3  <- getConstVal(a$cav$Temperature, "Tfm3")

if(length(a$cav$Uncertainty) > 0){
    withU <- TRUE
    uTuhv <- getConstVal(a$cav$Uncertainty, "uncertTch")
    uTfm  <- getConstVal(a$cav$Uncertainty, "uncertTfm")
}
N     <- length(Tfm3)

if(withU){
    df    <- data.frame(
        Tpbox = Tpbox,
        Troom = Troom,
        Txhv  = Txhv,
        Tuhv  = Tuhv,
        Tfm3  = Tfm3,
        uTuhv = uTuhv,
        uTfm = uTfm,
        Mpkt  = 1:N)
    
    ndf <- melt(df, id=c("Mpkt", "uTuhv","uTfm"))
}else{
    df    <- data.frame(
        Tpbox = Tpbox,
        Troom = Troom,
        Txhv  = Txhv,
        Tuhv  = Tuhv,
        Tfm3  = Tfm3,
        Mpkt  = 1:N)
    
    ndf <- melt(df, id=c("Mpkt"))
}
