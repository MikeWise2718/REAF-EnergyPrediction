detailPlot3 <- function(qdf,rparms,graphpath, vname,wdf = NULL,bdf = NULL,xlim=NULL,ylim=NULL,dval=F,saveplot=T) {
  if (!is.null(ylim)){
    qdf$val <- pmax( ylim[1], pmin( qdf$val, ylim[2]))
  }
  qdf <- qdf[!is.na(qdf$val), ]
  
  # Add missing weather
  if (!is.null(wdf)){
    wncdf <- wdf[!complete.cases(wdf),c(1,2)]
    wncdf$lev <- rep(0,nrow(wncdf))
    wncdf$dn <-
      trunc(as.numeric(1 + difftime(wncdf$wtime, rparms$pltstime, units = "days")))
    wncdf$wn <- trunc(wncdf$dn / 7)
    wncdf$wkrow <- trunc(wncdf$wn / 4)
    wncdf$wkcol <- trunc(wncdf$wn %% 4)
    wncdf$timewk <-
      (as.numeric(wncdf$wtime)  - wncdf$wn * 7 * 24 * 60 * 60 - as.numeric(rparms$pltstime) + 1) /
      (24 * 60 * 60)
    wncdf$series <- rep("no-weather-data",nrow(wncdf))
  }
  
  if (!is.null(bdf)){
    # Add holidays
    hodf <- bdf[bdf$holi > 0,c(1,2)]
    hodf$lev <- rep(0,nrow(hodf))
    hodf$dn <-
      trunc(as.numeric(1 + difftime(hodf$time, rparms$pltstime, units = "days")))
    hodf$wn <- trunc(hodf$dn / 7)
    hodf$wkrow <- trunc(hodf$wn / 4)
    hodf$wkcol <- trunc(hodf$wn %% 4)
    hodf$timewk <-
      (as.numeric(hodf$time)  - hodf$wn * 7 * 24 * 60 * 60 - as.numeric(rparms$pltstime) + 1) /
      (24 * 60 * 60)
    hodf$series <- rep("holiday",nrow(hodf))
  }
  
  
  # Add Month date lines
  vmax <- max( qdf$val,na.rm=T )
  vmin <- min( qdf$val,na.rm=T )
  print(sprintf("Value range min:%.4f  max:%.4f",vmin,vmax))
  
  vmax9 <- 0.9 * vmax
  atlist <- c()
  vldata <- NULL
  for (y in 2013:2015) {
    iy <- y - 2000
    for (m in 1:12) {
      dt <- ymd(sprintf("%d-%d-1",y,m),tz = "US/Pacific")
      dn <- trunc(as.numeric(1 + difftime(dt, rparms$pltstime, units = "days")))
      wn <- trunc(dn / 7)
      wr <- trunc(wn / 4)
      wc <- trunc(wn %% 4)
      twk <-
        (as.numeric(dt)  - wn * 7 * 24 * 60 * 60 - as.numeric(rparms$pltstime) + 1) / (24 *
                                                                                         60 * 60)
      jdf <- data.frame(wkrow = wr,wkcol = wc,timewk = twk)
      vldata <- rbind(vldata,jdf)
      aldf <-
        data.frame(
          timewk = twk + 0.25, val = vmax9, vlab = "month-start",
          lab = sprintf("%d-%d",iy,m),wkrow = wr,wkcol = wc
        )
      atlist[[length(atlist) + 1]] <- aldf
    }
  }
  ecolors <- c("blue","green","red","black","darkgreen","darkblue")
  names(ecolors) <- c("predicted","train","test","month-start","no-weather-data","holiday")
  #print(sprintf("vname:%s - %s",vname,ecolors[vname]))
  #cvname <- ecolors[vname]
  fp <- ggplot(qdf,aes(timewk,val,color=vlab)) +
    
    geom_point(size = 0.01,shape = 16,na.rm=T ) +
    
    facet_grid(wkrow ~ wkcol) +
    scale_color_manual(values = ecolors) +
    geom_vline(aes(xintercept = timewk),vldata,size = 0.01) +        # month markers
    labs(title=sprintf("%s - %s",rparms$bld,vname),y=vname,x="Time")  +
    theme_bw()  
  
  if (!is.null(wdf)){
    fp <- fp + geom_point(
      aes(timewk,lev),size = 1,data = wncdf,shape = 16,alpha = I(0.5),na.rm=T
    )  # missing weather
  }
  if (!is.null(bdf)){
    fp <- fp + geom_point(
      aes(timewk,lev),size = 1,data = hodf,shape = 16,alpha = I(0.5),na.rm=T
    )  # holidays
  }
  if (!is.null(xlim)){
    fp <- fp + scale_x_continuous( limits=xlim )
  }  
  if (!is.null(ylim)){
    fp <- fp + scale_y_continuous( limits=ylim )
  }
  ## Add month date lines to plot
  jnk <-
    sapply(atlist,function(x) {
      # have to assign one level up, thus the double <<-
      fp <<- fp + geom_text(data = x,aes(timewk,val),size = 2,label = x$lab)
      #print(sprintf("adding %s",x$lab))
    })
  #glbfp <<- fp
  if (saveplot){
    gfname <- sprintf("%s/wkpanels-%s-%s-%.2f.png",graphpath,rparms$bld,rparms$bchtime,rparms$trainfrac)
    #ggsave(file=gfname, plot=fp, width=rparms$gwidth, height=rparms$gheight)
    png(gfname,width = 2000,height = 4000)
    #  print(fp)
    jnk <- dev.off()
  }
  return(fp)
}