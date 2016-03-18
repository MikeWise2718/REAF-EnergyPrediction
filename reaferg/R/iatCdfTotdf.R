iatCdfTotdf <- function(cdf){
  
  # Line 1  
  dy <- (72-69)
  t1df <- data.frame(x=cdf$eetime,y=(72.5-69)*1.05 + 69)
  t1df$label <- sprintf("avg occ:%.3f iat:%.3f",cdf$avgocc,cdf$avgiat)
  t1df$hjust <- 1
  t1df$size <- 2
  t1df$color <- "norm"
  t1df$ph.hours <- cdf$ph.hours
  t1df$ph.level <- cdf$ph.level
  t1df$avgiat <- cdf$avgiat
  
  idx <- which(t1df$avgiat==max(t1df$avgiat))
  t1df[idx,]$color <- "high"
  idx <- which(t1df$avgiat==min(t1df$avgiat))
  t1df[idx,]$color <- "low"
  t1df$avgiat <- NULL
  
  
  tdf <- rbind(t1df)
  return(tdf)
}