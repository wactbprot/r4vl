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

df.pressure.fill.offs <- data.frame(
    pfill.offset = pfill.offset,
    pdpC = pdpC,
    Mpkt = 1:N)

ndf.pressure.fill.offs <- melt(df.pressure.fill.offs, id=c("Mpkt"))

if(withU){
    df.pressure.fill <- data.frame(
        plw = plw,
        pfill = pfill,
        upfill = upfill,
        Mpkt = 1:N)

    df.pressure.cal <- data.frame(
        pcal = pcal,
        poffset = poffset,
        pcorrind = pcorrind,
        upcal = upcal,
        Mpkt = 1:N)

    ndf.pressure.cal <- melt(df.pressure.cal,
                             id=c("Mpkt","upcal","poffset"))
    ndf.pressure.fill <- melt(df.pressure.fill,
                              id=c("Mpkt","upfill"))

}else{
    df.pressure.fill <- data.frame(
        plw = plw,
        pfill = pfill,
        Mpkt = 1:N)

    df.pressure.cal <- data.frame(
        pcal = pcal,
        poffset = poffset,
        pcorrind = pcorrind,
        Mpkt = 1:N)

    ndf.pressure.cal <- melt(df.pressure.cal,
                             id=c("Mpkt","poffset"))
    ndf.pressure.fill <- melt(df.pressure.fill,
                              id=c("Mpkt"))

}
N <- length(pcal)



        pltfill <- ggplot(ndf.pressure.fill)

        pltfill <- pltfill + geom_point(aes(x = Mpkt,
        y = value,
        color = factor(ndf.pressure.fill$variable)),
        size=5)

        pltfill <- pltfill + guides(color = guide_legend("Pressure"))

        pltfill <- pltfill + scale_y_log10()

        pltfill <- pltfill + theme(legend.position="bottom")

        if(withU){
        pltfill <- pltfill + geom_errorbar(aes(x=Mpkt,
        ymax = pfill*(1 + upfill),
        ymin = pfill*(1 - upfill)))
        }

        ###

        pltfilloffs <- ggplot(ndf.pressure.fill.offs)

        pltfilloffs <- pltfilloffs + geom_point(aes(x = Mpkt,
        y = value,
        color = factor(ndf.pressure.fill.offs$variable)),
        size=5)

        pltfilloffs <- pltfilloffs + guides(color = guide_legend("Pressure"))

        pltfilloffs <- pltfilloffs + theme(legend.position="bottom")

        ###

        pltcal <- ggplot(ndf.pressure.cal)
        pltcal <- pltcal + geom_point(aes(x = Mpkt,
        y = value,
        color = factor(ndf.pressure.cal$variable)),
        size=5)

        pltcal <- pltcal + guides(color = guide_legend("Pressure"))

        pltcal <- pltcal + scale_y_log10()

        pltcal <- pltcal + theme(legend.position="bottom")

        if(withU){
        pltcal <- pltcal + geom_errorbar(aes(x=Mpkt,
        ymax = pcal*(1 + upcal),
        ymin = pcal*(1 - upcal)))
        }


