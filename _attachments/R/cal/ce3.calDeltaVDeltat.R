ce3.calDeltaVDeltat <- function(ccc){
  msg <- "calculated by ce3.calDeltaVDeltat"

  tUnit   <- "s"
  vUnit   <- "l"
  a       <-  abbrevList(ccc)
  DRIFT   <-  getSubList(a$cm,"drift_slope_x")
  drift   <-  getConstVal(NA,NA,DRIFT)

  A       <- getConstVal(a$cms, "fbv_A")  
  B       <- getConstVal(a$cms, "fbv_B")
  C       <- getConstVal(a$cms, "fbv_C")
  
  f <- function(x){
    return(A*x^3/3 + A*B*x^2 + x*(A*B^2 + C))
  }
  

  
  t2mm    <- getConstVal(a$cms,"turn_2_mm")
  ms2s    <- getConstVal(a$cc,"ms_2_s")
  
  noOfSZ  <-  length(drift)
  
  for(i in 0:(noOfSZ - 1)){
    ## delta t
    stype <- paste("slope_x_",i,sep="")
    ptype <- paste("mean_p_",i,sep="")
    ttype <- paste("mean_t_",i,sep="")

    mp    <- getConstVal(a$cm, ptype)
    
    MT    <- getSubList(a$cm, ttype)
    mt    <- getConstVal(NA,NA,MT)
    
    SLOPE      <- getSubList(a$cm,stype)
    slope      <- getConstVal(NA,NA,SLOPE)
    
    pconv      <- getConvFactor(ccc,SLOPE$Unit, DRIFT$Unit)
    corrSlope  <- slope - drift[i+1] * pconv
    ci         <- mp -  corrSlope * mt
    t0         <- -ci/corrSlope
    tconv      <- getConvFactor(ccc,tUnit, MT$Unit)
    nt         <- length(t0)
    j1         <- 1:(nt-1)
    j2         <- j1 + 1
    
    deltat     <- (t0[j2] - t0[j1]) * tconv #

    ## delta V
    turntype   <- paste("turn_",i,sep="")
    h          <- abs(getConstVal(a$cm, turntype)) * t2mm
    nv          <- length(h)
    
    ## f(x) = ax^3/3+a*b*x^2+x*(a*b^2+c)
    ## A(f(x[2] - f(x1))/(x[2] - x[1]
    i1 <- 1:(nv-1)
    i2 <- i1 + 1

    deltaV <- abs(f(h[i2]) - f(h[i1]))
    A      <- deltaV/abs(h[i2] - h[i1])
    vconv  <- getConvFactor(ccc,vUnit, "mm^3")

    L <- deltaV * vconv/ deltat
    print(L)  
  }
  

  return(ccc)
}
