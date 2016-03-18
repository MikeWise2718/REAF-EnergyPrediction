overviewPlot3 <- function(qdf,rparms,vname,xlim=NULL,ylim=NULL,dval=F){
  if (!is.null(ylim)){
    qdf$val <- pmax( ylim[1], pmin( qdf$val, ylim[2]))
  }
  ecolors <- c("blue","green","red","black","darkgreen","darkblue")
  names(ecolors) <- c("predicted","train","test","month-start","no-weather-data","holiday")
  qqdf <- qdf[!is.na(qdf$val), ]
  qp <- qplot(time,val,data=qqdf,color=I(ecolors[qqdf$vlab]),alpha=I(1/10)) +
    labs(title=sprintf("%s - %s",rparms$bld,vname),y=vname,x="Time") + 
    scale_color_manual(values=ecolors) +
    guides(colour = guide_legend(override.aes = list(alpha = 1)))  # override legend alpha
  if (!is.null(xlim)){
    qp <- qp + scale_x_continuous( limits=xlim )
  }  
  if (!is.null(ylim)){
    qp <- qp + scale_y_continuous( limits=ylim )
  }
  # print(qp)
  return(qp)
}