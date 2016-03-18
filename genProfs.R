library(ggplot2)
library(reshape2)
library(randomForest)
library(gridExtra)
library(grid)
library(lubridate)

lagvar <- function (y,n){
  ny <- length(y)
  lagy <- c(y[1],y[1:(ny-1)])
}

set.seed(12345)

ecolors <- c("blue","green3","red","black","darkgreen","darkblue")
names(ecolors) <- c("predicted","train","test","month-start","no-weather-data","holiday")


trimddf <- function(ddf,deldata,tzab){
  mdf <- ddf
  for (li in deldata){
    sd <- as.POSIXct(li$s,tzab)
    ed <- as.POSIXct(li$e,tzab)
    mdf <- mdf[ mdf$time<sd | ed<mdf$time, ]
  }
  return(mdf)
}


deldatalst <- list(
  list(s="2013-02-01",e="2013-04-01"),
  list(s="2013-09-01",e="2013-10-01"),
  list(s="2013-03-01",e="2013-05-01"),
  list(s="2014-10-01",e="2014-11-01")
)

# r2convplotold <- function(bld,depvar,rsq)
# {
#   nrsq <- length(rsq)
#   cdf <- data.frame(iter=1:nrsq,rsq=rsq)
#   miny <- min(cdf$rsq)
#   r2plot <- ggplot(cdf,aes(iter,rsq)) + 
#     geom_line(color="blue") + 
#     geom_point(color="red") + 
#     scale_y_continuous(limits=c(miny,1)) +
#     labs(title=sprintf("%s %s R-squared convergence",bld,depvar))
#   return(r2plot)
# }
# 
# doimpplotold <- function(bld,depvar,impmatrix){
#   idf <- data.frame(impmatrix)
#   idf$vars <- rownames(idf)
#   idf$vars <- factor(idf$vars,levels=idf$vars[order(idf$X.IncMSE)])
#   g1 <- ggplot(idf,aes(x=vars)) + 
#     geom_bar(aes(y=X.IncMSE),stat="identity",color=I("black"),fill=I("gray80")) + 
#     coord_flip() + 
#     labs(title=sprintf("%s - %s Inc MSE",bld,depvar))
#   idf$vars <- factor(idf$vars,levels=idf$vars[order(idf$X.IncMSE)])
#   g2 <- ggplot(idf,aes(x=vars)) + 
#     geom_bar(aes(y=IncNodePurity),stat="identity",color=I("black"),fill=I("gray80")) + 
#     coord_flip() + 
#     labs(title=sprintf("%s IncNodePurity",depvar))
#   impplot <- suppressWarnings(arrangeGrob(g1,g2,ncol=2))
# }

genErgProf <- function(bld,depvar,ybase=0,yrng=1,ymax=9e99,ymin=-9e99,trainfak=0.7,rfntree=10,csdate="2013-01-01",cedate="2013-06-30",tzab="US/Pacific"){
  sdate <- as.POSIXct(csdate,tz=tzab)
  edate <- as.POSIXct(cedate,tz=tzab) + 24*60*60 - 15*60 # end of the day
  print(depvar)
  
  nsec <- as.numeric(difftime(edate,sdate,units="secs"))
  nQhrs <- nsec / (15*60)

  nQhrsPerDay <- 24*4
  x <- 1:nQhrs/nQhrsPerDay
  nDays <- nQhrs / nQhrsPerDay
  wx <- rep(runif(nDays,0.9,1.0),length.out=nQhrs,each=nQhrsPerDay) # daily noise
  wcx<- rep(c(1,1,1,0.9,0.4,0.4,1),length.out=nQhrs,each=nQhrsPerDay) # weekly cycle
  y <- sin(2*pi*x) + 0.4*sin(4*pi*x) + rnorm(nQhrs,0,0.1) 
  maxy <- max(abs(y))
  y <- y/maxy 
  y <- yrng*y*wx*wcx + ybase
  y <- pmin(y,ymax)
  y <- pmax(y,ymin)
  
  
  stime <- sdate
  ddf <- data.frame(x,val=y,vlab="train")
  ddf$time <- stime + (0:(nQhrs-1))*15*60
  ddf$year <- year(ddf$time)
  ddf$daynum <- trunc(as.numeric(1+difftime(ddf$time, stime, units="days")))
  ddf$weeknum <-trunc(ddf$daynum / 7)
  ddf$wkrow <- trunc(ddf$weeknum / 4)
  ddf$wkcol <- ddf$weeknum %% 4
  ddf$timewk <- (as.numeric(ddf$time)-ddf$weeknum*7*24*3600-as.numeric(stime) + 1)/(24*3600)
  ddf$holi <- 0 # no holidays

  ddf$ly1 <- lagvar(ddf$val)
  ddf$ly2 <- lagvar(ddf$ly1)

  ddf$dy <- ddf$ly1-ddf$ly2
  
  ddf$fx <- ddf$x-trunc(ddf$x)
  ddf$ix <- trunc(ddf$x)
  

  # trim away our missing values here
  print("  preping")
  mdf <- trimddf(ddf,deldatalst,tzab)
  trnlim <- trunc(trainfak*nrow(mdf))
  print(sprintf("  tf:%.1f nrow:%d  tlim:%d",trainfak,nrow(mdf),trnlim))
  mdf$vlab <- "test"
  mdf[ 1:trnlim, ]$vlab <- "train"
  trndf <- mdf[  1:trnlim,]
  tstdf <- mdf[-(1:trnlim),]

  print("  rf")
  rf <- randomForest( val~ fx+ix+ly1+ly2,importance=T, ntree=rfntree,trndf)
  ndf <- ddf
  ndf$val <- predict(rf,ddf)
  ndf$vlab <- "predicted"
  pdf <- rbind(ndf,mdf)
  ddf$predval <- ndf$val

  # roll our own variable importance plot
  impplot <- doimpplot(bld,depvar,rf$importance)

  # roll our own R2 convergence plot
  r2plot <- dor2convplot(bld,depvar,rf$rsq)

  # fake a overview plot
  print("  overview")
  tit <- sprintf("%s - %s",bld,depvar)
  overviewplot <- ggplot(pdf,aes(time,val,color=vlab,fill=vlab))+
    geom_point(alpha=0.1)+
    scale_color_manual(values = ecolors) +
    labs(title=tit)

  # fake a detail plot
  print("  detail")
  detailplot <- ggplot(pdf,aes(timewk,val,color=vlab)) +
    geom_point(size = 0.01,shape = 16,na.rm=T ) +
    facet_grid(wkrow ~ wkcol) +
    scale_color_manual(values = ecolors) +
#    geom_vline(aes(xintercept = timewk),vldata,size = 0.01) +        # month markers
    labs(title=tit,y=depvar,x="Time") + 
    theme_bw()  

  rv <- list()
  rv$bld <- bld
  rv$rf <- rf
  rv$ddf <- ddf
  rv$r2plot <-   r2plot
  rv$impplot <-  impplot
  rv$overviewplot <- overviewplot
  rv$detailplot <- detailplot
  rv$depvar <- depvar
  return(rv)
}

genPrediction <- function(train,csdate="2013-03-01",cedate="2013-03-07",tzab="US/Pacific"){
  
  sdate <- as.POSIXct(csdate,tz=tzab)
  edate <- as.POSIXct(cedate,tz=tzab) + 24*60*60 - 15*60 # end of the day
  # fake a prediction plot
  print(sprintf("%s - %s",train$bld,train$depvar))
  ddf <- train$ddf
  pdf <- ddf[sdate<=ddf$time & ddf$time <=edate, ]  
  pdf$predicted <- predict(train$rf,pdf)
  mdf <- melt(pdf,measure.vars=c("val","predicted"))
  mdf$variable <- as.character(mdf$variable)
  mdf[ mdf$variable=="val", ]$variable <- "train"
  mdf$variable <- factor(mdf$variable,levels=c("train","predicted"))
  rv <- list()
  rv$predplot <- ggplot(data=mdf,aes(x=time,y=value,color=variable))+
                   scale_color_manual(values = ecolors) +
                   geom_line()+
                   labs(title=train$name)
  rv$mdf <- mdf
  return(rv)
}

trainAll <- function(bld="City Faker",tf=0.7,rfn=10){
  rv <- list()
  rv[["occ"]] <-  genErgProf(bld,"occ",trainfak=tf,rfntree=rfn,ybase=0.8,yrng=0.8,ymin=0,ymax=1)
  rv[["iat"]] <-  genErgProf(bld,"iat",trainfak=tf,rfntree=rfn,ybase=72,yrng=3)
  rv[["diat"]] <-  genErgProf(bld,"diat",trainfak=tf,rfntree=rfn,ybase=0,yrng=3)
  rv[["e.base"]] <- genErgProf(bld,"e.base",trainfak=tf,rfntree=rfn,ybase=300,yrng=100)
  rv[["e.tot"]] <- genErgProf(bld,"e.tot",trainfak=tf,rfntree=rfn,ybase=1300,yrng=300)
  return(rv)
}

predictAll <- function(tsets,csdate,cedate){
  rv <- list()
  rv[["occ"]] <-  genPrediction(tsets[["occ"]],csdate,cedate)
  rv[["iat"]] <-  genPrediction(tsets[["iat"]],csdate,cedate)
  rv[["diat"]] <-  genPrediction(tsets[["diat"]],csdate,cedate)
  rv[["e.base"]] <- genPrediction(tsets[["e.base"]],csdate,cedate)
  rv[["e.tot"]] <- genPrediction(tsets[["e.tot"]],csdate,cedate)
  return(rv)
}


testMe <- function(){
  tsets <- trainAll("Fity Faker",trainpct=50,rfn=15)
  trn <<- tsets[["e.tot"]]
  
  grid.draw(trn$impplot)
  print(trn$overviewplot)
  print(trn$detailplot)
  
  psets <- pedictAll(tsets,csdate="2013-01-01",cedate="2013-01-07")
  pred <<- psets[["e.tot"]]
  print(pred$predplot)
}
