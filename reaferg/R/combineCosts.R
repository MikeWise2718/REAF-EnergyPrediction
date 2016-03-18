combineCosts <- function(cdf,rv){
  ddf <- data.frame(peakerg=rv$peakerg,sumerg=rv$sumerg)
  ddf$dc <- rv$dc
  ddf$uc <-  rv$uc
  ddf$tc <-  rv$tc
  ddf$avgiat <- rv$avgiat
  ddf$avgocc <- rv$avgocc
  ddf$ph.start <- rv$preheat$start
  ddf$ph.hours <- rv$preheat$hours
  ddf$ph.level <- rv$preheat$level
  ddf$sstime <- rv$sstime
  ddf$eetime <- rv$eetime
  
  if (is.null(cdf)){
    cdf <- ddf
  } else {
    cdf <- rbind(cdf,ddf)
  }
  rownames(cdf) <- NULL
  return(cdf)
}