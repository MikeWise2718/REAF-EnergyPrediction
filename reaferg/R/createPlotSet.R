createPlotSet <- function(rdf,rdfvname,rdfvlab,trn,trnvname,trnvlab,tst,tstvname,tstvlab,orgtime,tzab="UCT"){
  n1df <- data.frame(time=rdf$time,qhr=rdf$qhr,val=rdf[[rdfvname]],vlab=rdfvlab)
  n2df <- data.frame(time=trn$time,qhr=trn$qhr,val=trn[[trnvname]],vlab=trnvlab)
  n3df <- data.frame(time=tst$time,qhr=tst$qhr,val=tst[[tstvname]],vlab=tstvlab)
  rdf <- rbind(n1df,n2df,n3df)
  rdf$time <- ymd_hms(rdf$time,tz=tzab)
  #  attr(rdf$time,"tzone") <- tzab
  rdf$year <- year(rdf$time)
  rdf$daynum <- trunc(as.numeric(1+difftime(rdf$time, orgtime, units="days")))
  rdf$weeknum <-trunc(rdf$daynum / 7)
  rdf$wkrow <- trunc(rdf$weeknum / 4)
  rdf$wkcol <- rdf$weeknum %% 4
  rdf$timewk <- (as.numeric(rdf$time)  - rdf$weeknum*7*24*60*60 - as.numeric(orgtime) + 1)/(24*60*60)
  rdf$holi <- 0 # no holidays
  return(rdf)
}
