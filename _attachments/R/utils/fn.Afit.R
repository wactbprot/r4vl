## Menzers Integral über
## A(x) = A(x+B)(x+B)+C
fn.Afit <- function(cf, x){
  return(cf$A*x^3/3 + cf$A*cf$B*x^2 + x*(cf$A*cf$B^2 + cf$C))
}
