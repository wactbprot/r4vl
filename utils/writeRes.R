writeRes <- function( ccc ){
  msg <- "set by writeRes"
  ## - data reduction as given by ToDo
  ## - round
  
  a <- abbrevList(ccc)
  if(length(a$cp) > 0 &
     length(a$cpt) > 0){
      reType <- a$cpt$Type
      
      ## cal
      pcal <- getSubList(a$cav$Pressure, "cal")
      pcal$HeadCell <- "$p_{cal}$"
      pcal$UnitCell <- paste("in", pcal$Unit)
      ## ind
      pind <- getSubList(a$cav$Pressure, "ind")
      pind$HeadCell <- "$p_{ind}$"
      pind$UnitCell <- paste("in", pind$Unit)
      ## ind_offset
      pindoffs <- getSubList(a$cav$Pressure, "ind_offset")
      pindoffs$HeadCell <- "$p_r$"
      pindoffs$UnitCell <- paste("in", pindoffs$Unit)
      ## ind_corr
      pindcorr <- getSubList(a$cav$Pressure, "ind")
      pindcorr$HeadCell <- "$p_r$"
      pindcorr$UnitCell <- paste("in", pindcorr$Unit)

      if(reType =="error"){
          ## rel
          erel <- getSubList(a$cav$Error, "relative")
          erel$HeadCell <- "$e$"
          erel$UnitCell <- paste("in", erel$Unit)

          ## ---
          ## sort out function
          ## ---

          ccc$Calibration$Result$Table[[1]] <- pcal
          ccc$Calibration$Result$Table[[2]] <- pind 
          ccc$Calibration$Result$Table[[3]] <- pindoffs
          ccc$Calibration$Result$Table[[4]] <- pindcorr
          ccc$Calibration$Result$Table[[5]] <- erel  
                    
      }
      
  }

return(ccc)

}
