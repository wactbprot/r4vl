## 4403 bezieht sich auf die TC#
##
fn.4403 <- function(cf, x){
  return(cf$a + cf$b * x + cf$c*x^2 + cf$d * (log(x))^2 + cf$e *log(x)/x^2 )
}
