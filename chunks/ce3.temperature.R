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

if(length(iout) > 0){
    Tpbox <- Tpbox[-iout]
    Troom <- Troom[-iout]
    Txhv  <- Txhv[-iout]
    Tuhv  <- Tuhv[-iout]
    Tfm3  <- Tfm3[-iout]

    if(length(a$cav$Uncertainty) > 0){
        uTuhv <- uTuhv[-iout]
        uTfm  <- uTfm[-iout]
    }

}

N     <- length(Tfm3)

if(withU){
    df.temperature    <- data.frame(
        Tpbox = Tpbox,
        Troom = Troom,
        Txhv  = Txhv,
        Tuhv  = Tuhv,
        Tfm3  = Tfm3,
        uTuhv = uTuhv,
        uTfm = uTfm,
        Mpkt  = 1:N)

    ndf.temperature <- melt(df.temperature, id=c("Mpkt", "uTuhv","uTfm"))
}else{
    df.temperature    <- data.frame(
        Tpbox = Tpbox,
        Troom = Troom,
        Txhv  = Txhv,
        Tuhv  = Tuhv,
        Tfm3  = Tfm3,
        Mpkt  = 1:N)

    ndf.temperature <- melt(df.temperature, id=c("Mpkt"))
}

        plt <- ggplot(ndf.temperature)

        plt <- plt + geom_point(aes(x = Mpkt,
        y = value,
        color = factor(ndf.temperature$variable)),
        size=5)

        plt <- plt +    geom_hline(yintercept = upperLimit,
        lwd = 1,
        linetype = "dotted",
        color="red")
        plt <- plt +    geom_hline(yintercept = lowerLimit,
        lwd = 1,
        linetype = "dotted",
        color="red" )

        plt <- plt +    geom_hline(yintercept = aim,
        lwd = 1,
        linetype = "longdash",
        color="darkgreen")
        if(withU){
        plt <- plt +    geom_errorbar(aes(x=Mpkt,
        ymax = Tfm3*(1 + uTfm),
        ymin = Tfm3*(1 - uTfm)), width=0.1)

        plt <- plt +    geom_errorbar(aes(x=Mpkt,
        ymax = Tuhv*(1 + uTuhv),
        ymin = Tuhv*(1 - uTuhv)), width=0.1)
        }

        plt <- plt + theme(legend.position="bottom")
        plt <- plt + guides(color = guide_legend("Temperatures"))

