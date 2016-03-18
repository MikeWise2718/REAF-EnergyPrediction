r2print <- function(titwrd,y,predy,nrt,npr){
  if (is.factor(y)){
    y <- as.numeric(as.character(y))
  }
  if (is.factor(predy)){
    predy <- as.numeric(as.character(predy))
  }
  meany <- mean(y,na.rm=T)
  res <- y-predy
  ssres <- sum(res*res,na.rm=T)
  sstot <- sum((y-meany)*(y-meany),na.rm=T)
  
  r2test <- 1 - (ssres/sstot)
  adjrat <- (nrt-1)/(nrt-npr-1)
  print(sprintf("%s regressors:%d  predictors:%d   adjrat:%.5f",titwrd,nrt,npr,adjrat))
  print(sprintf("   SSres:%.5f  SStot:%.5f  mean(y):%.5f",ssres,sstot,meany)) # See Wikipedia "Coefficent of determination"" entry
  ar2test <- 1 - ((1-r2test)*adjrat)
  print(sprintf("%s R2 test:%.5f   Adjusted R2 test%.5f",titwrd, r2test,ar2test))
  return(ar2test)
}