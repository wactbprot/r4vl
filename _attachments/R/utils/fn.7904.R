## s. wiki (todo)
## 7904 bezieht sich auf die TC#
##
fn.7904 <- function(cf, x){
  return((cf$a + cf$c * x + cf$e*x^2)/(1 + cf$b * x + cf$d * x^2 +  cf$f * x^3))
}
### Fehler zwischen den commits:
##
## fa122f2603d2df00f7d5a45e575d026084a7c872
                                        # und
## 0c6ebe69929ed772458393878df0eb2e4df6b131
## an Stelle d wurde b benutzt
