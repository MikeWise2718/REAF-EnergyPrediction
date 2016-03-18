trainAndPredict_3 <- function(rparms,qpath,vname,vfname=NULL,iatlims=NULL,lmnames=NULL,dropvars=NULL,fullset=F,medianFilter=F,filterDates=T,addlaggediat=F,occAsFactor=F){

  print(sprintf("trainAndPredict - %s stime:%s  etime:%s",rparms$bld,rparms$stime,rparms$etime))

  ergv <- rparms$ergname
  rrdf <- readData3(rparms$bld,qpath,vname,vfname,rparms$tzab)
  
  if (occAsFactor){
    occfak <- factor(rrdf$occ)
    maxlevs <- 20
    if (length(levels(occfak)>maxlevs)){
      discocc <- trunc( 0.5 + rrdf$occ*maxlevs )/maxlevs
      occfak <- factor(discocc)
    }
    rrdf$occ <- occfak
  }
  if (!is.null(ergv)){
    rrdf[["erg"]] <- rrdf[[ergv]]
  }
  rrdf$wday <- mwk(rrdf$wday)
  if (medianFilter){
    rrdf[[vname]] <- medianFilter(rrdf[[vname]])
  }
  #print(summary(rrdf))
  rrdf[["e.tot"]] <- NULL
  rrdf[["e.base"]] <- NULL
  rrdf[["e.plug"]] <- NULL
  if (addlaggediat){
    nv <- nrow(rrdf)
    fval <- rrdf$iat[1]
    # lagged iat
    rrdf$liat <- c(fval,rrdf$iat[1:(nv-1)])
    # recalc the derivative to be sure about its value
    rrdf$diat <- rrdf$iat-rrdf$liat
    # post lag
    lval <- rrdf$iat[nv]
    rrdf$piat <- c(rrdf$iat[2:nv],lval)
    vname <- "piat"
    rrdf$iat <- NULL
  }
  
  rdf <- rrdf[ complete.cases(rrdf), ]
  if (filterDates){
    rdf <- rdf[ rparms$stime<=rdf$time & rdf$time<=rparms$etime, ]
  }
  
  rlst <- splitData(rdf,rparms$bld,vname,tfrac=rparms$trainfrac,runname=rparms$runname)
  trndf <- rlst[[1]]
  tstdf <- rlst[[2]]
  
  if (occAsFactor){
    trndf <- droplevels(trndf)
  }
  #  browser()
  if (!is.null(trndf$iat)){
    qiat <-  quantile(trndf$iat,c(0.20,0.80),na.rm=T)
    qmed <- median(trndf$iat)
    print("qiat")
    print(qiat)
    iatlims <- round(qiat,1)
    print("iatlims")
    print(iatlims)
  }
  
  rfvals <- doRf(rparms,trndf,vname,dropvars )
  rffit <- rfvals$rf

  predvname <- paste0("pred.",vname)
  
  rdf[[predvname]] <- predict(rffit,rdf)
  tstdf[[predvname]] <- predict(rffit,tstdf)
  trndf[[predvname]] <- predict(rffit,trndf)
  rrdf[[predvname]] <- predict(rffit,rrdf)
  
  vlab <- paste0(rparms$bld,"-",vname)
  
  # etot.r2 <- r2print("e.tot",test$e.tot,test$res,nrow(test),ncol(test)-1)
  r2trn <- r2print(sprintf("trn-%s",vlab),trndf[[vname]],trndf[[predvname]],nrow(trndf),ncol(trndf)-1)
  r2tst <- r2print(sprintf("tst-%s",vlab),tstdf[[vname]],tstdf[[predvname]],nrow(tstdf),ncol(tstdf)-1)
  
  # browser()
  pdf <-  createPlotSet(rdf, predvname,"predicted",trndf,vname,"train",tstdf,vname,"test",rparms$pltstime,rparms$tzab)
  prdf <- createPlotSet(rrdf,predvname,"predicted",trndf,vname,"train",tstdf,vname,"test",rparms$pltstime,rparms$tzab)
  
  
  rlst <- list()
  rlst[["trndf"]] <- trndf
  rlst[["tstdf"]] <- tstdf
  rlst[["rdf"]] <- rdf
  rlst[["prdf"]] <- prdf
  rlst[["rffit"]] <- rffit
  rlst[["slmfit"]] <- NULL
  rlst[["iatlims"]] <- iatlims
  # rlst[["qmed"]] <- qmed
  rlst[["r2plot"]] <- rfvals$r2plot
  rlst[["impplot"]] <- rfvals$impplot
  return(rlst)
}