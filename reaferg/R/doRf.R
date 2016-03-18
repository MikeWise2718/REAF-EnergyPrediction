doRf <- function(rparms,trn,vname,dropvars=NULL){
  set.seed(5678)
  rfntree <- rparms$rfntree
  print(sprintf("Starting rf on %s-%s",rparms$bld,vname))
  algelap <- 0
  
  
  algstarttime <- Sys.time()
  # Always drop these because we do not want to predict on them
  trn$qhr <- NULL
  trn$time <- NULL
  trn$datetime <- NULL
  #  trn$year <- NULL
  #browser()
  
  if (!is.null(dropvars)){
    print("dropvars:")
    print(dropvars)
    print("names(trn):")
    print(names(trn))
    idxes <- match(dropvars,names(trn))
    idxes <- idxes[ !is.na(idxes) ]
    #     print("match:")
    #     print(idxes)
    trn[,idxes] <- list(NULL)
    print("dropping vars")
    print(dropvars)
  }
  formc <- sprintf("%s ~ .",vname)
  print(sprintf("Training with %s",formc))
  form <- as.formula(formc)
  rf <- randomForest(form, trn, do.trace=10, ntree=rfntree,importance=TRUE)
  algelap <- as.numeric((Sys.time()-algstarttime)[1],units="secs")
  # print(rf)
  print(sprintf("%s Training rf at %s took %.1f secs",vname,algstarttime,algelap))
  # print(summary(rf))
  #   if (!is.null(rf$rsq)){
  #     r2plot <- qplot(1:length(rf$rsq),rf$rsq,main=sprintf("%s - R2 by iteration",vname))
  #     print(r2plot)
  #   } else {
  #     print(sprintf("rf$rsq is NULL"))
  #     r2plot <- NULL
  #   }
  #browser()
  #print("importance of rf")
  #impplot <- importance(rf)
  #print("print(impplot)")
  #print(impplot)
  #print("varimpplot")
  #varImpPlot(rf,main=sprintf("%s - rf varImpPlot",vname))
  
  # roll our own variable importance plot
  impplot <- doimpplot(rparms$bld,vname,rf$importance)
  
  # roll our own R2 convergence plot
  r2plot <- dor2convplot(rparms$bld,vname,rf$rsq)
  
  rparms$parms$algelap <- algelap
  rparms$parms$rfntree <- rfntree
  rvals <- list()
  rvals[['impplot']] <- impplot
  rvals[['r2plot']] <- r2plot
  rvals[['rf']] <- rf
  return(rvals)
}