doPlots <- function(ddf,odf,edf,tdf,rparms,mode){
  
  g1 <- doOnePlot(rparms, ddf$time,ddf$pred.cas.occ,sprintf("pred %s occ",mode),ddf$iatseg,
                   ddf$time,ddf$orig.cas.occ,"act occ","gray80",ddf$ph.hours,ddf$ph.level,
                   sprintf("%s occ - pred %s vs. act",rparms$bld,mode),
                   "time","occ",lines=T,tdf=odf,ybreaks=c(0.25,0.5,0.75,1.0))  
  
  g2 <- doOnePlot(rparms,ddf$time,ddf$pred.cas.erg,sprintf("pred %s %s",mode,rparms$ergname),ddf$iatseg,
                  ddf$time,ddf$orig.cas.erg,"act erg","gray80",ddf$ph.hours,ddf$ph.level,
                  sprintf("%s %s - pred %s vs. act", rparms$bld,rparms$ergname,mode),
                  "time",rparms$ergname,lines=T,tdf=edf)
  g3 <- NULL
  #   g3 <- doOnePlot(rparms,ddf$time,ddf$pred.cas.diat,sprintf("pred %s diat",mode),ddf$iatseg,
  #           ddf$time,ddf$orig.cas.diat,"act diat","gray80",ddf$ph.hours,ddf$ph.level,
  #           sprintf("%s diat pred %s vs. act",rparms$bld,mode),
  #           "time","delta temp",lines=T)  
  
  g4 <- doOnePlot(rparms,ddf$time,ddf$pred.cas.iat,sprintf("pred %s iat",mode),ddf$iatseg,
                  ddf$time,ddf$orig.cas.iat,"act iat","gray80",ddf$ph.hours,ddf$ph.level,
                  sprintf("%s iat pred %s vs. act",rparms$bld,mode),
                  "time","indoor air temp",lines=T,tdf=tdf)  
  rv <- list()
  rv$occ <- g1
  rv$erg <- g2
  rv$diat <- g3
  rv$iat <- g4
  return(rv)
}