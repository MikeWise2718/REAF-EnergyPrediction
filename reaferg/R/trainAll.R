trainAll <- function(rparms){
  
  rv <- list()
  epath <- rparms$epath
  feats <- rparms$feats
  
  depvar <- "occ"
  occlst <- trainAndPredict_3(rparms,epath$qdata,depvar,
                              dropvars=c("iat",feats$wetr,"ydayf","holi","year","hdcnt"),
                              occAsFactor = T)
  og <- overviewPlot3(occlst[["prdf"]],rparms,depvar)
  dg <- NULL #detailPlot(occlst[["prdf"]],rparms,"occ")
  occlst$overviewplot <- og
  occlst$detailplot <- dg
  occlst$depvar <- depvar
  rv[[depvar]] <- occlst
  
  depvar <- "erg"
  erglst <- trainAndPredict_3(rparms,epath$qdata,depvar,vfname="erg3",dropvars=NULL)
  og <- overviewPlot3(erglst[["prdf"]],rparms,rparms$ergname)
  dg <- detailPlot3(erglst[["prdf"]],rparms,epath$graph,rparms$ergname)
  erglst$overviewplot <- og
  erglst$detailplot <- dg
  erglst$depvar <- depvar
  rv[[depvar]] <- erglst
  
  depvar <- "diat"
  diatlst <- trainAndPredict_3(rparms,epath$qdata,depvar,vfname="diat3",iatlim=iatborders,lmnames=rparms$hvaclevs,
                               dropvars=c(feats$time,feats$bld,feats$dwetr,feats$pred),
                               medianFilter=F,occAsFactor = F)
  og <- overviewPlot3(diatlst[["prdf"]],rparms,depvar,ylim=c(-0.25,0.25))
  dg <- detailPlot3(diatlst[["prdf"]],rparms,epath$graph,depvar,ylim=c(-0.25,0.25))
  diatlst$overviewplot <- og
  diatlst$detailplot <- dg
  diatlst$depvar <- depvar
  rv[[depvar]] <- diatlst
  
  depvar <- "iat"
  iiatlst <- trainAndPredict_3(rparms,epath$qdata,depvar,vfname="diat3",lmnames=rparms$hvaclevs,
                               dropvars=c(feats$time,feats$bld,feats$dwetr,feats$pred),
                               medianFilter=F,addlaggediat=T,occAsFactor = F)
  og <- overviewPlot3(iiatlst[["prdf"]],rparms,depvar,ylim=c(-0.25,0.25))
  dg <- detailPlot3(iiatlst[["prdf"]],rparms,epath$graph,depvar,ylim=c(-0.25,0.25))
  iiatlst$overviewplot <- og
  iiatlst$detailplot <- dg
  iiatlst$depvar <- depvar
  rv[[depvar]] <- iiatlst
  return(rv)
}