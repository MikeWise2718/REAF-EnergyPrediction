initVer <- function(){
  ver <- list()
  ver$number <- 0.32
  ver$string <- sprintf("Version %.2f",ver$number)
  ver$starttime <- Sys.time()
  ver$startfmttime <- sprintf(format(ver$starttime, "%d %b %Y - %H:%M:%S"))
  return(ver)
}
