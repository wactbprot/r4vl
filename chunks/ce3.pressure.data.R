withU         <- FALSE

plw           <- getConstVal(a$cav$Pressure, "lw")
pfill         <- getConstVal(a$cav$Pressure, "fill")
pfill.offset  <- getConstVal(a$cav$Pressure, "fill_offset")
pdpC          <- getConstVal(a$cav$Pressure, "dpC")
pind          <- getConstVal(a$cav$Pressure, "ind")
pcal          <- getConstVal(a$cav$Pressure, "cal")


try(pcorrind      <- getConstVal(a$cav$Pressure, "ind_corr"), TRUE)
try(pcorrind      <- getConstVal(a$cav$Pressure, "corrind"), TRUE)

try(poffset       <- getConstVal(a$cav$Pressure, "offset"), TRUE)
try(poffset       <- getConstVal(a$cav$Pressure, "ind_offset"), TRUE)


if(length( a$cav$Uncertainty) > 0){
    withU         <- TRUE
    upfill        <- getConstVal(a$cav$Uncertainty, "uncertPfill")
    upcal         <- getConstVal(a$cav$Uncertainty, "uncertPcal_rel")
}

N <- length(pcal)

dffilloffs <- data.frame(
	pfill.offset = pfill.offset, 
	pdpC         = pdpC,	
	Mpkt         = 1:N)

ndffilloffs <- melt(dffilloffs, id=c("Mpkt"))

if(withU){
    dffill    <- data.frame(
        plw          = plw,
        pfill        = pfill,
        upfill       = upfill,
        Mpkt         = 1:N)
    
    dfcal    <- data.frame(
        pcal         = pcal,
        poffset      = poffset,
        pcorrind     = pcorrind,
        upcal        = upcal,
        Mpkt         = 1:N)

    ndfcal <- melt(dfcal, id=c("Mpkt","upcal"))
    ndfill <- melt(dffill, id=c("Mpkt","upfill"))
    
}else{
    dffill    <- data.frame(
        plw          = plw,
        pfill        = pfill,
        Mpkt         = 1:N)
    
    dfcal    <- data.frame(
        pcal         = pcal,
        poffset      = poffset,
        pcorrind     = pcorrind,
        Mpkt         = 1:N)

    ndfcal <- melt(dfcal, id=c("Mpkt"))
    ndfill <- melt(dffill, id=c("Mpkt"))
   
}
N             <- length(pcal)
