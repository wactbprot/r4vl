getOutIndex <- function(ccc){

  a <- abbrevList(ccc)

  ## es ist wichtig, dass append(outIndex, _case_)
  ## benutzt wird. Es gehen sonst Fälle verloren

  ## NB:
  ## > append(NULL, c(1,2))
  ## [1] 1 2
  ## >
  outIndex <- NULL


  ##
  ## hier können noch weitere Bedingungen form. werden,
  ## die zum Verwerfen von Punkten führen
  ## die grenze 1e-10 ist etwas wilkürlich aber (hoffentlich) sicher
  ##
  ## 1.)
  if(a$cs =="CE3"){
    if(length(a$cmv) > 0){

      Ldv <- getConstVal(a$cmv, "cfm3")
      outIndex <- append(outIndex,which(Ldv < 1e-10))

    }
    ## 2.)
    ## ...


    ## 3.)
    ## ...
  }#CE3


  if(a$cs =="DKM|FRS5" | a$cs =="FRS5" ){
    if(length(a$cmv) > 0){

      TFRS   <- getSubList(a$cmv$Temperature, "keithley_ch110")
      Tfrs        <- getConstVal(NA,NA,TFRS)    *  getConvFactor(ccc,"C",TFRS$Unit)  # C

      outIndex <- append(outIndex,
                         which((Tfrs > 30) & (Tfrs < 20)))

      pdcrOff  <- getConstVal(a$cma,"frs_res_off")
      pdcr     <- getConstVal(a$cmv,"frs_res")
      outIndex <- append(outIndex,
                         which(is.na(pdcr - pdcrOff)))

      RFRS    <- getConstVal(a$cmv,    "frs_p") ## in lb
      RFRSZc  <- getConstVal(a$cmv,    "frs_zc_p") ## in lb
      RFRSZc0 <- getConstVal(a$cma,    "frs_zc0_p") ## in lb

      RM <-       RFRS - (RFRSZc - RFRSZc0)

      outIndex <- append(outIndex,
                         which( is.na(RM) |  RM < 0.02 | RM > 12 ))

    }
  }#DKM|FRS5

  if(a$cs =="SE1"){
    ##1.)
    ## pfill hat NAs

    N <- length(a$cmscex)

    checkVec <- rep(FALSE,N)

    ruskaPfill       <- getConstVal(a$cmv, "ruska_p_fill")
    ruskaPfillOffset <- getConstVal(a$cmv, "ruska_p_fill_offset")
    cdgPfill         <- getConstVal(a$cmv, "cdg_p_fill")
    cdgPfillOffset   <- getConstVal(a$cmv, "cdg_p_fill_offset")

    ruskaI <- which(!is.na(ruskaPfill - ruskaPfillOffset))
    cdgI <- which(!is.na(cdgPfill - cdgPfillOffset))

                                        #bisher wird nur das ruska benutzt
    checkVec[ruskaI] <- TRUE
                                        # checkVec[cdgI]   <- TRUE

    outIndex <- append(outIndex,which(!checkVec))


    ## 2.)
    ## pind hat NAs 7.2.11
    OFF <- getSubList(a$cmv,  "p_ind_offset")
    IND <- getSubList(a$cmv, "p_ind" )

    off <-  getConstVal(NA,NA,OFF)
    ind <- getConstVal(NA,NA,IND)


    outIndex <- append(outIndex,which(is.na(ind - off)))

  }

  if(!is.list(ccc$Calibration$Measurement$SequenceControl)){
    ccc$Calibration$Measurement$SequenceControl<-
      as.list(ccc$Calibration$Measurement$SequenceControl)
  }

  if(length(outIndex) > 0){

    ccc$Calibration$Measurement$SequenceControl$outIndex <-  outIndex
  }else{
    ccc$Calibration$Measurement$SequenceControl$outIndex <- c(0)
  }
  return(ccc)

  }

