##'
##' 2115 bezieht sich auf die TC#
##'
fn.2115 <- function(cf, x){
  return(cf$a + cf$b * x + cf$c * sqrt(x)*log(x)  + cf$d *sqrt(x))
}
