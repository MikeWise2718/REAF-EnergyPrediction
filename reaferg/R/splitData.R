splitData <- function(rdf,blld,vname,tfrac=0.7,runname="run",testonlastweeks=T,writeOutData=F){
  if (testonlastweeks){
    lstidx <- trunc(tfrac*nrow(rdf))
    istrainpt <- seq(1:nrow(rdf))<lstidx
    print(sprintf("%s testonlastweeks set - last training index:%d",vname,lstidx))
  } else {
    istrainpt <- 2<ranidx  # 92.5 percent training cases
  }
  rdf$datetime <- rdf$time
  trn <- rdf[istrainpt,]
  tst <- rdf[!istrainpt,]
  rdf$datetime <- NULL
  trn$datetime <- NULL
  tst$datetime <- NULL
  
  #  browser()
  if (is.factor(rdf[[vname]])){
    for (lev in levels(rdf[[vname]])){
      ntrn <- sum(trn[[vname]]==lev)
      ntst <- sum(tst[[vname]]==lev)
      # print(sprintf("%s level:%s ntrn:%d ntst:%d",vname,lev,ntrn,ntst))
      if (ntrn==0 & ntst!=0){
        #   print(sprintf("shifting level:%s  ntrn:%d ntst:%d",lev,ntrn,ntst))
        idx <- which(tst[[vname]]==lev)
        # move one of the tst records to trn
        sidx <- sample(idx,1)
        trn <- rbind(trn,tst[sidx,])
        tst <- tst[-sidx,]
      }
    }
  }
  
  print(sprintf("Spliting %s - train cases:%d test cases:%d",vname,nrow(trn),nrow(tst)))
  
  if (writeOutData){
    trfnamebase <- sprintf("train-%s-%s-%s-%.1f.csv",blld,vname,runname,version)
    trfname <- sprintf("%s/%s",tdatapath,trfnamebase)
    print(sprintf("Writing %d rows to %s",nrow(trn),trfname))
    write.csv(trn,trfname,quote=F,row.names=F)
    
    tsfnamebase <- sprintf("test-%s-%s-%s-%.1f.csv",blld,vname,runname,version)
    tsfname <- sprintf("%s/%s",tdatapath,tsfnamebase)
    print(sprintf("Writing %d rows to %s",nrow(tst),tsfname))
    write.csv(tst,tsfname,quote=F,row.names=F)
  }
  rlst <- list()
  rlst[[1]] <- trn
  rlst[[2]] <- tst
  print("end of splitdata")
  return(rlst)
}
