##'
##' 3132 bezieht sich auf die TC#
##'
fn.3132 <- function(cf, x){
  return(cf$a + cf$b * (log(x))^2 + cf$c * log(x)  + cf$d *log(x)/x)
}
