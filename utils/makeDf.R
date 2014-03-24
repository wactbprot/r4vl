#' genDf takes a list and returns a df representation
#' of it
makeDf <- function(sheetLists){

    if(length(sheetLists$Type) > 0){
        ## print(sheetLists$Type)
        n               <- length(sheetLists$Value)
        mat             <- matrix(ncol = 1,
                                  nrow = n)
        headDescr        <- c(paste( sheetLists$Type ,
                                    "in",
                                    sheetLists$Unit,
                                    sep = " "))

        mat[1:n,] <- getValVect(sheetLists$Value)
    }else{
        NS    <-  length(sheetLists)
        n     <- length(sheetLists[[1]]$Value)
        mat   <-  matrix(ncol=NS,
                         nrow = n)

        headDescr <- rep("", NS)
        for(j in 1:NS){
            headDescr[j] <- paste( sheetLists[[j]]$Type ,
                                  "in",
                                  sheetLists[[j]]$Unit,
                                  sep =" ")
            mat[1:n,j]  <- getValVect(sheetLists[[j]]$Value)
        }
        ##        dimnames(x) <- list(row_names, col_names)


    }

    dimnames(mat) <- list(1:n, headDescr)
    return(as.data.frame(mat))
    
}
