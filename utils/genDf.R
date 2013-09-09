#' genDf takes a list and returns a df representation
#' of it
makeDf <- function(sheetList){

         if(length(av[[sheetName]]$Type) > 0){
            //print(av[[sheetName]]$Type)
            mat <- matrix(ncol=1, nrow=length(av[[sheetName]]$Value) + 3)
            
        }else{
            NS  <-  length(av[[sheetName]])
            mat <-  matrix(ncol=NS, nrow=length(av[[sheetName]][[1]]$Value) + 3)
            for(j in 1:NS)){
            //print(av[[sheetName]][[j]]$Type)
                
            }
         print(mat)
        }
 
                    
                                        #return(df)
    }
}
