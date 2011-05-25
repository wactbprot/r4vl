dispatchResCal <- function(ccc){

  a <- abbrevList(ccc)

  if(length(a$cp) > 0){

    if(length(a$cpt) > 0){

      resType <- a$cpt$Type
      ## Problem: welches sigma?
      ## Antwort s. Functionsname!
      ## also vom customCo
       if(resType == "sigma"){
         print("call calSigma ")
         ccc <-  calSigma(ccc)
       }

      if(resType == "error"){
        print("call calError ")
        ccc <-  calError(ccc)

       }

       if(resType == "ratio"){
         print("call calRatio ")
         ccc <-  calRatio(ccc)

       }

       if(resType == "correctionFactor"){
         print("call calCf ")
         ccc <-  calCf(ccc)
       }

    }
  }
  return(ccc)
}
