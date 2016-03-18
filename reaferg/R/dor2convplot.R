dor2convplot <- function(bld,depvar,rsq)
{
  nrsq <- length(rsq)
  if (nrsq==0){
    return(NULL)
  }
  cdf <- data.frame(iter=1:nrsq,rsq=rsq)
  miny <- min(cdf$rsq)
  r2plot <- ggplot(cdf,aes(iter,rsq)) + 
    geom_line(color="blue") + 
    geom_point(color="red") + 
    scale_y_continuous(limits=c(miny,1)) +
    labs(title=sprintf("%s %s R-squared convergence",bld,depvar))
  return(r2plot)
}