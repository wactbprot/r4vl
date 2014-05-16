dispatchResSum <- function( ccc ){
    msg <- "set by writeRes"
    ## - data reduction as given by ToDo
    ## - round
    
    a <- abbrevList(ccc)

    if(length(a$cp)                         > 0 &
       length(a$cpt)                        > 0 &
       length(a$cpt$Values$Pressure)        > 0 &
       length(a$cpt$Values$Pressure$Value)  > 0 &
       length(pcal)                         > 3){

        reType  <- a$cpt$Type

        if(reType =="srg_error"){
            ccc <- resSrgError(ccc)
        }

        if(reType =="error"){
            ccc <- resError(ccc)
        }

        if(reType =="sens"){
            ccc <- resSens(ccc)
        }

    return(ccc)
}
