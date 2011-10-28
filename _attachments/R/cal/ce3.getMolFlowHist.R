ce3.getMolFlowHist <- function(cdb){
  
  cdb$design <- "rproc"
  cdb$view   <- "ce3_flow_hist"
  gas        <- "N2"
  res        <- cdbGetView(cdb)$res
  
  resPfill <- NULL
  resCond  <- NULL
  resTemp  <- NULL
  resDate  <- NULL
  
  for(i in 1:res$total_rows){
    
    if(res$rows[[i]]$value$Gas == gas ){
      
      tmpCond  <- getSubList(res$rows[[i]]$value$Conductance, "cfm3")$Value
      lc       <- length(tmpCond)
      
      tmpPfill <- getSubList(res$rows[[i]]$value$Pressure, "fill")$Value
      tmpTemp  <- getSubList(res$rows[[i]]$value$Temperature, "Tfm3")$Value
      
      
      
      ##   takeThis <- which((tmpPfill <  1.2) & (tmpPfill > 0))
      
      if (length(tmpPfill) ==lc &
          length(tmpTemp)  ==lc &
          lc >0){
        
        tmpDate <- rep(res$rows[[i]]$value$Date,lc)
        
        resPfill <- append(resPfill,tmpPfill)
        resCond <- append(resCond,tmpCond)
        resTemp <- append(resTemp,tmpTemp)
        resDate <- append(resDate,tmpDate)
      }
    }
  }
  return(list(pfill=resPfill,
              conductance=resCond,
              temperature=resTemp,
              date = resDate))
  
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
