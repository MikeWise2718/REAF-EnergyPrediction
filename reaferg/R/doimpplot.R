doimpplot <- function(bld,depvar,impmatrix){
  idf <- data.frame(impmatrix)
  idf$vars <- rownames(idf)
  #browser()
  if (is.null(idf$X.IncMSE)){
    return(NULL)
  }
  idf$vars <- factor(idf$vars,levels=idf$vars[order(idf$X.IncMSE)])
  g1 <- ggplot(idf,aes(x=vars)) + 
    geom_bar(aes(y=X.IncMSE),stat="identity",color=I("black"),fill=I("gray80")) + 
    coord_flip() + 
    labs(title=sprintf("%s - %s Inc MSE",bld,depvar))
  idf$vars <- factor(idf$vars,levels=idf$vars[order(idf$X.IncMSE)])
  g2 <- ggplot(idf,aes(x=vars)) + 
    geom_bar(aes(y=IncNodePurity),stat="identity",color=I("black"),fill=I("gray80")) + 
    coord_flip() + 
    labs(title=sprintf("%s IncNodePurity",depvar))
  impplot <- suppressWarnings(arrangeGrob(g1,g2,ncol=2))
}
