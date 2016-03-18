mwk <- function(x){
  rv <- (x+6)%%7
  rv <- ifelse(rv==0,7,rv)
  return(rv)
}