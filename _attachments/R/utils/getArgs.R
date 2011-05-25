getArgs <- function(agrs){
	
a <- list()

a$host   <- args[1]
a$DBName <- args[2]
a$id 	 <- args[3]

  return(a)
}
