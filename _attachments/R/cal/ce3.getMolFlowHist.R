ce3.getMolFlowHist <- function(cdb){

  cdb$design <- "map"
  cdb$view   <- "ce3-get_flow_by_date"

  res        <- cdbGetView(cdb)$res

  resPfill <- NULL
  resCond  <- NULL
  resTemp  <- NULL
  resDate  <- NULL
  resGas  <- NULL

  for(i in 1:length(res$rows)){

    tmpCond  <- getSubList(res$rows[[i]]$value$Conductance, "cfm3")$Value
    lc       <- length(tmpCond)

    tmpPfill <- getSubList(res$rows[[i]]$value$Pressure, "fill")$Value
    tmpTemp  <- getSubList(res$rows[[i]]$value$Temperature, "Tfm3")$Value



    ##   takeThis <- which((tmpPfill <  1.2) & (tmpPfill > 0))

    if (length(tmpPfill) ==lc &
        length(tmpTemp)  ==lc &
        lc >0){

      tmpDate <- rep(res$rows[[i]]$value$Date,lc)
      tmpGas  <- rep(res$rows[[i]]$value$Gas,lc)

      resPfill <- append(resPfill,tmpPfill)
      resCond <- append(resCond,tmpCond)
      resTemp <- append(resTemp,tmpTemp)
      resDate <- append(resDate,tmpDate)
      resGas <- append(resGas,tmpGas)

    }

  }
  return(list(pfill=resPfill,
              conductance=resCond,
              temperature=resTemp,
              date = resDate,
              gas  = resGas))

}

## nur zur Anregung
## library(rgl)
##  xp <- (x-min(x))/max(x-min(x))
##  yp <- (y-min(y))/max(y-min(y))
##  zp <- (z-min(z))/max(z-min(z))
##  rgl.close()
##  open3d()
##  points3d(xp,yp,zp)
##  title3d('main','sub','xlab','ylab','zlab')
##  grid3d()
