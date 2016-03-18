occCdfTotdf <- function(cdf){
  
  # Line 1  
  t1df <- data.frame(x=cdf$eetime,y=1.0*1.12)
  t1df$label <- sprintf("avg occ:%.3f iat:%.3f",cdf$avgocc,cdf$avgiat)
  t1df$hjust <- 1
  t1df$size <- 2
  t1df$color <- "norm"
  t1df$ph.hours <- cdf$ph.hours
  t1df$ph.level <- cdf$ph.level
  t1df$avgocc <- cdf$avgocc
  
  idx <- which(t1df$avgocc==max(t1df$avgocc))
  t1df[idx,]$color <- "high"
  idx <- which(t1df$avgocc==min(t1df$avgocc))
  t1df[idx,]$color <- "low"
  t1df$avgocc <- NULL
  
  
  tdf <- rbind(t1df)
  return(tdf)
}