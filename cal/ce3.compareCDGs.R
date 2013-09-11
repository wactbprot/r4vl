ce3.compareCDGs <- function(ccc){
    msg    <- "calculated by ce3.compareCDGs"
    a      <- abbrevList(ccc)

    gas    <- a$cmscg
    if(gas == "Ar" || gas == "N2"){
        g <- gas
    }else{
        g <- "N2"
        msg <- paste(msg, "no calibration for CDGA for gas: ", gas, ". Use N2 instead")
    }


    ## Range u.ä. ist im recipe
    ## festgelegt
    pcdga    <-  getConstVal(a$cma$Pressure, "cdga_comp") #10T
    pcdgb    <-  getConstVal(a$cma$Pressure, "cdgb_comp") #1000T
    p0cdga   <-  getConstVal(a$cma$Pressure, "cdga_comp_offset")
    p0cdgb   <-  getConstVal(a$cma$Pressure, "cdgb_comp_offset")

    if((length(pcdga)  > 0) &
       (length(pcdgb)  > 0) &
       (length(p0cdga) > 0) &
       (length(p0cdgb) > 0) ){

        cfcdga    <- list()
        cfcdga$a  <- getConstVal(  a$cmco, paste("cdgaCorrA_",g,sep=""))
        cfcdga$b  <- getConstVal(  a$cmco, paste("cdgaCorrB_",g,sep=""))
        cfcdga$c  <- getConstVal(  a$cmco, paste("cdgaCorrC_",g,sep=""))
        cfcdga$d  <- getConstVal(  a$cmco, paste("cdgaCorrD_",g,sep=""))
        cfcdga$e  <- getConstVal(  a$cmco, paste("cdgaCorrE_",g,sep=""))
        cfcdga$f  <- getConstVal(  a$cmco, paste("cdgaCorrF_",g,sep=""))

        cfcdgb    <- list()
        cfcdgb$a  <- getConstVal(  a$cmco, "cdgbCorrA")
        cfcdgb$b  <- getConstVal(  a$cmco, "cdgbCorrB")
        cfcdgb$c  <- getConstVal(  a$cmco, "cdgbCorrC")
        cfcdgb$d  <- getConstVal(  a$cmco, "cdgbCorrD")
        cfcdgb$e  <- getConstVal(  a$cmco, "cdgbCorrE")
        cfcdgb$f  <- getConstVal(  a$cmco, "cdgbCorrF")

        pa        <- pcdga - p0cdga
        pb        <- pcdgb - p0cdgb
        dpfill    <-  (pa/(fn.7904(cfcdga,pa ) + 1))/(pb/(fn.7904(cfcdgb, pb) +1)) - 1


        ccc$Calibration$Analysis <- checkSetList(
            ccc$Calibration$Analysis)

        ccc$Calibration$Analysis$AuxValues <- checkSetList(
            ccc$Calibration$Analysis$AuxValues)


        ccc$Calibration$Analysis$AuxValues$Pressure <-
            setCcl(ccc$Calibration$Analysis$AuxValues$Pressure,
                   "dpfill",
                   "1",
                   dpfill,
                   paste(msg, "; dpfill =  (pcdga  - p0cdga)/(pcdgb  - p0cdgb) -1") )
    }
    return(ccc)
}
