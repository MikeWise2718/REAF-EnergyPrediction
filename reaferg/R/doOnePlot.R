doOnePlot <- function(rparms,s1.x, s1.y, s1.lab, s1.iatseg,
                             s2.x, s2.y, s2.lab, s2.color,
                             ph.h, ph.l,
                             maintit,xlab,ylab,lines=F,tdf=NULL,ybreaks=NULL)
{
  q1df <- data.frame(x = s1.x, y = s1.y, lab=s1.iatseg,ph.hours=ph.h,ph.level=ph.l)
  q2df <- data.frame(x = s2.x, y = s2.y, lab=s2.lab,   ph.hours=ph.h,ph.level=ph.l)
  
  qdf <- rbind(q1df,q2df)
  qdf <- qdf[ order(qdf$x), ]
  
  # for now disconnect these labels
  qdf$lab <- as.character(qdf$lab)
  idx <- qdf$lab=="heating" | qdf$lab=="cooling"
  if (sum(idx)>0){
    qdf[ idx,]$lab <- "drift"
  }
  
  qcolors        <- c("darkblue","darkblue","darkblue",s2.color,"darkred","darkblue","darkgreen")
  names(qcolors) <- c("heating","drift","cooling",s2.lab,"high","norm","low")
  
  gp <- ggplot(qdf, aes(x,y,color=lab) ) +
    
    geom_point(alpha = I(0.5),na.rm=T) +
    
    scale_color_manual(values = qcolors) +
#    labs(title=maintit,y=ylab,x=xlab) + 
    labs(title=maintit,x="",y="") + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
    facet_grid( ph.hours ~ ph.level ) +
    guides( colour=F )+
    scale_x_datetime(labels = date_format("%y-%m-%d %R %Z",tz=rparms$tzab))
  if (lines){
    gp <- gp + geom_line(alpha = I(0.5),na.rm=T)
  }
  if (!is.null(tdf)){
    #print(tdf)
    gp <- gp + geom_text(data=tdf,aes(x=x,y=y,label=label,color=color,hjust=hjust))
    ggp <<- gp
  }
  if (!is.null(ybreaks)){
    gp <- gp + scale_y_continuous(breaks=ybreaks) 
  }
  return(gp)
}