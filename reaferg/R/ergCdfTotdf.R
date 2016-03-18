ergCdfTotdf <- function(cdf){
  
  # Line 1  
  t1df <- data.frame(x=cdf$eetime,y=max(cdf$peakerg)*1.04)
  t1df$label <- sprintf("d:%.0f u:%.0f t: %.0f",cdf$dc,cdf$uc,cdf$tc)
  t1df$hjust <- 1
  t1df$size <- 2
  t1df$color <- "norm"
  t1df$ph.hours <- cdf$ph.hours
  t1df$ph.level <- cdf$ph.level
  t1df$tc <- cdf$tc
  
  idx <- which(t1df$tc==max(t1df$tc))
  t1df[idx,]$color <- "high"
  idx <- which(t1df$tc==min(t1df$tc))
  t1df[idx,]$color <- "low"
  t1df$tc <- NULL
  
  # Line 2  
  t2df <- data.frame(x=cdf$eetime,y=max(cdf$peakerg)*0.99)
  t2df$label <- sprintf("pk:%.0f ",cdf$peakerg)
  t2df$hjust <- 1
  t2df$size <- 2
  t2df$color <- "norm"
  t2df$ph.hours <- cdf$ph.hours
  t2df$ph.level <- cdf$ph.level
  t2df$peakerg <- cdf$peakerg
  
  idx <- which(t2df$peakerg==max(t2df$peakerg))
  t2df[idx,]$color <- "high"
  idx <- which(t2df$peakerg==min(t2df$peakerg))
  t2df[idx,]$color <- "low"
  t2df$peakerg <- NULL
  
  # Line 2  
  t3df <- data.frame(x=cdf$eetime,y=max(cdf$peakerg)*0.94)
  t3df$label <- sprintf("sm:%.0f ",cdf$sumerg)
  t3df$hjust <- 1
  t3df$size <- 2
  t3df$color <- "norm"
  t3df$ph.hours <- cdf$ph.hours
  t3df$ph.level <- cdf$ph.level
  t3df$sumerg <- cdf$sumerg
  
  idx <- which(t3df$sumerg==max(t3df$sumerg))
  t3df[idx,]$color <- "high"
  idx <- which(t3df$sumerg==min(t3df$sumerg))
  t3df[idx,]$color <- "low"
  t3df$sumerg <- NULL
  
  
  tdf <- rbind(t1df,t2df,t3df)
  return(tdf)
}