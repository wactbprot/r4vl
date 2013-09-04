withU <- FALSE

plw <- getConstVal(a$cav$Pressure, "lw")
pfill <- getConstVal(a$cav$Pressure, "fill")
pfill.offset <- getConstVal(a$cav$Pressure, "fill_offset")
pdpC <- getConstVal(a$cav$Pressure, "dpC")
pind <- getConstVal(a$cav$Pressure, "ind")
pcal <- getConstVal(a$cav$Pressure, "cal")


try(pcorrind <- getConstVal(a$cav$Pressure, "ind_corr"), TRUE)
try(pcorrind <- getConstVal(a$cav$Pressure, "corrind"), TRUE)

try(poffset <- getConstVal(a$cav$Pressure, "offset"), TRUE)
try(poffset <- getConstVal(a$cav$Pressure, "ind_offset"), TRUE)


if(length( a$cav$Uncertainty) > 0){
    withU <- TRUE
    upfill <- getConstVal(a$cav$Uncertainty, "uncertPfill")
    upcal <- getConstVal(a$cav$Uncertainty, "uncertPcal_rel")
}

if(length(iout) > 0){
    poffset      <-  poffset[-iout]
    pcorrind     <-  pcorrind[-iout]
    plw          <-  plw[-iout]
    pfill        <-  pfill[-iout]
    pfill.offset <-  pfill.offset[-iout]
    pdpC         <-  pdpC[-iout]
    pind         <-  pind[-iout]
    pcal         <-  pcal[-iout]
    if(length( a$cav$Uncertainty) > 0){

        upfill <- upfill[-iout]
        upcal <-  upcal[-iout]
    }

}



N <- length(pcal)

dffilloffs <- data.frame(
    pfill.offset = pfill.offset,
    pdpC = pdpC,
    Mpkt = 1:N)

ndffilloffs <- melt(dffilloffs, id=c("Mpkt"))

if(withU){
    dffill <- data.frame(
        plw = plw,
        pfill = pfill,
        upfill = upfill,
        Mpkt = 1:N)

    dfcal <- data.frame(
        pcal = pcal,
        poffset = poffset,
        pcorrind = pcorrind,
        upcal = upcal,
        Mpkt = 1:N)

    ndfcal <- melt(dfcal, id=c("Mpkt","upcal"))
    ndfill <- melt(dffill, id=c("Mpkt","upfill"))

}else{
    dffill <- data.frame(
        plw = plw,
        pfill = pfill,
        Mpkt = 1:N)

    dfcal <- data.frame(
        pcal = pcal,
        poffset = poffset,
        pcorrind = pcorrind,
        Mpkt = 1:N)

    ndfcal <- melt(dfcal, id=c("Mpkt"))
    ndfill <- melt(dffill, id=c("Mpkt"))

}
N <- length(pcal)

pltfill <- ggplot(ndfill)
pltfill <- pltfill + geom_point(aes(x = Mpkt,
                                    y = value,
                                    color = factor(ndfill$variable)),
                                size=5)
pltfill <- pltfill + guides(color = guide_legend("Pressure"))
pltfill <- pltfill + scale_y_log10()
pltfill <- pltfill + theme(legend.position="bottom")

if(withU){
    pltfill <- pltfill + geom_errorbar(aes(x=Mpkt,
                                           ymax = pfill*(1 + upfill),
                                           ymin = pfill*(1 - upfill)))
}



pltfilloffs <- ggplot(ndffilloffs)
pltfilloffs <- pltfilloffs + geom_point(aes(x = Mpkt,
                                            y = value,
                                            color = factor(ndffilloffs$variable)),
                                        size=5)
pltfilloffs <- pltfilloffs + guides(color = guide_legend("Pressure"))
pltfilloffs <- pltfilloffs + theme(legend.position="bottom")
