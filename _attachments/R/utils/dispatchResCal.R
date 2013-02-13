dispatchResCal <- function(ccc){
  a <- abbrevList(ccc)
  if(length(a$cp) > 0){
    if(length(a$cpt) > 0){

      resType <- a$cpt$Type

      if(resType == "sigma"){
        ccc <-  calSigma(ccc)
       }
      
      if(resType == "error"){
        ccc <-  calError(ccc)
       }

       if(resType == "ratio"){
         ccc <-  calRatio(ccc)
       }

       if(resType == "correctionFactor"){
         ccc <-  calCf(ccc)
       }
    }
  }
  return(ccc)
}
