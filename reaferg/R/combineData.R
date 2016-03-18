combineData <- function(adf,rv){
  ddf <- rv$casdf
  ddf$ph.start <- rv$preheat$start
  ddf$ph.hours <- rv$preheat$hours
  ddf$ph.level <- rv$preheat$level
  ddf <- ddf[ order(ddf$time), ]
  rownames(ddf) <- NULL
  if (is.null(adf)){
    adf <- ddf
  } else {
    adf <- rbind(adf,ddf)
  }
  rownames(adf) <- NULL
  return(adf)
}