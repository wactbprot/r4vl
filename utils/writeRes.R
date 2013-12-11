writeRes <- function( ccc ){
  msg <- "set by writeRes"
  ## - data reduction as given by ToDo
  ## - round
  
  a <- abbrevList(ccc)
  if(length(a$cp) > 0 &
     length(a$cpt) > 0){
      reType <- a$cpt$Type
      
      ## cal
      pcal <- getSubList(a$cav$Pressure, "cal"),
      pcal$HeadLine <- "$p_{cal}$"
      pcal$UnitLine <- paste("in", pcal$Unit)
      ## ind
      pind <- getSubList(a$cav$Pressure, "ind"),
      pind$HeadLine <- "$p_{ind}$"
      pind$UnitLine <- paste("in", pind$Unit)
      ## ind_offset
      pindoffs <- getSubList(a$cav$Pressure, "ind"),
      pindoffs$HeadLine <- "$p_r$"
      pindoffs$UnitLine <- paste("in", pindoffs$Unit)
      ## ind_corr
      pindcorr <- getSubList(a$cav$Pressure, "ind"),
      pindcorr$HeadLine <- "$p_r$"
      pindcorr$UnitLine <- paste("in", pindcorr$Unit)

      if(reType =="error"){
          ## rel
          prel <- getSubList(a$cav$Error, "relative"),
          prel$HeadLine <- "$e$"
          prel$UnitLine <- paste("in", prel$Unit)
          
          ## ---
          ## sort out function
          ## ---
          
          ccc$Calibration$Result$Values <- c(
              list(Pressure=
                   c(pcal,pind, pindoffs,pindcorr),
                   Error=
                   c(prel)))
                                 
      }
      
  }

return(ccc)

}
