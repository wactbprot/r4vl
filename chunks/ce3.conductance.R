withU         <- FALSE

cnom           <- getConstVal(a$cav$Conductance, "cnom")
sdcnom         <- getConstVal(a$cav$Conductance, "sd_cnom")
cfm3           <- getConstVal(a$cav$Conductance, "cfm3")
diffhist       <- getConstVal(a$cav$Conductance, "diff_hist")




iout <- which(abs(diffhist) > 0.1) # alles was mehr als 10% abweicht ist Schrott
if(length(iout) > 0){
cnom           <-  cnom[-iout]       
sdcnom         <-  sdcnom[-iout]    
cfm3           <-  cfm3[-iout]      
diffhist       <-  diffhist[-iout]  




if(length( a$cav$Uncertainty) > 0){
    withU     <- TRUE
    uDV       <- getConstVal(a$cav$Uncertainty, "uncertDeltaV")[-iout]
    uDVDt     <- getConstVal(a$cav$Uncertainty, "uncertDeltaVDeltat")[-iout]
    uDt       <- getConstVal(a$cav$Uncertainty, "uncertDeltat")[-iout]
}

}else{


if(length( a$cav$Uncertainty) > 0){
    withU     <- TRUE
    uDV       <- getConstVal(a$cav$Uncertainty, "uncertDeltaV")
    uDVDt     <- getConstVal(a$cav$Uncertainty, "uncertDeltaVDeltat")
    uDt       <- getConstVal(a$cav$Uncertainty, "uncertDeltat")
}
}

N <- length(cnom)


if(withU){
    df    <- data.frame(
        cnom      =   cnom,     
        sdcnom    =   sdcnom ,  
        cfm3      =   cfm3,     
        diffhist  =   diffhist, 
        uDV       =  uDV,    
        uDVDt     =  uDVDt,  
        uDt       =  uDt,    
        Mpkt       = 1:N)
    
    ndf <- melt(df, id=c("Mpkt","uDV","uDVDt","uDt","diffhist","sdcnom"))
    
}else{
    df    <- data.frame(
        cnom      =   cnom,     
        sdcnom    =   sdcnom,   
        cfm3      =   cfm3,     
        diffhist  =   diffhist, 
        Mpkt      = 1:N)
    
    ndf <- melt(df, id=c("Mpkt","diffhist","sdcnom"))
  
   
}
N             <- length(pcal)

pltc <- ggplot(ndf)
pltc <- pltc + geom_point(aes(x = Mpkt,
                                    y = value,
                                    color = factor(ndf$variable)),
                                size=5)
pltc <- pltc +    guides(color = guide_legend("Conductance"))
pltc <- pltc +    scale_y_log10()
pltc <- pltc +    theme(legend.position="bottom")
pltc <- pltc +  geom_errorbar(aes(x=Mpkt,
                                  ymax = cnom*(1 + sdcnom ),
                                  ymin = cnom*(1 - sdcnom)), col=1)

if(withU){
    pltc <- pltc +  geom_errorbar(aes(x=Mpkt,
                                      ymax = cnom*(1 + (uDV^2 +uDt^2 +uDVDt^2)^.5 ),
                                      ymin = cnom*(1 - (uDV^2 +uDt^2 +uDVDt^2)^.5)), col=2)
   
}
