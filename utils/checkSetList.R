checkSetList <- function(List){

  if(length(List) == 0){
    List <- resetList(List)
  }
  return(List)
}
