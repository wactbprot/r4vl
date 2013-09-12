#' genDf takes a list and returns a df representation
#' of it
makeDf <- function(sheetLists){
    
         if(length(sheetLists$Type) > 0){
            ## print(sheetLists$Type)
            mat <- matrix(ncol=1, nrow=length(sheetLists$Value) + 3)
            
        }else{
            NS  <-  length(sheetLists)
            mat <-  matrix(ncol=NS, nrow=length(sheetLists[[1]]$Value) + 3)
            for(j in 1:NS){
            ## print(sheetLists[[j]]$Type)
                
            }
         print(mat)
        }
 
                    
                                        #return(df)

}
