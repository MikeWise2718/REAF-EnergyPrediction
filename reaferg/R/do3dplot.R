do3dplot <- function(rparms,diatlst,ddf,mode,xlb,ylb,zlb,addActualPath=F,addPredPath=T){
  
  rdf <- diatlst$rdf
  rffit <- diatlst$rffit
  slmfit <- diatlst$slmfit
  iatlims <- diatlst$iatlims
  
  diatlim <- c(-0.25,0.25)
  iatrng <- c(68,77)
  
  exdf <- get_exdf3(rparms,rdf,slmfit,rffit)
  exdf$colseg <- rparms$hvaccolors[exdf$segname]
  
  ddf$pred.iat.minus.oat <- ddf$pred.iat-ddf$temp.f
  ddf$colseg <- rparms$hvaccolors[1]
  
  if (sum(ddf$iat>iatlims[1])>0 ){
    ddf[ ddf$iat>iatlims[1],]$colseg <- rparms$hvaccolors[2]
  }
  if (sum(ddf$iat>iatlims[2])>0 ){
    ddf[ddf$iat>iatlims[2] ,]$colseg <- rparms$hvaccolors[3]
  }
  
  y <- exdf$pred.lm.diat
  if (mode=="rf"){
    y <- exdf$pred.diat
  }
  plot3d(exdf$erg,y,exdf$iat,col=exdf$colseg,xlab=xlb,ylab=ylb,zlab=zlb)
  if (addPredPath){
    lines3d(ddf$pred.erg,ddf$pred.cas.diat,ddf$iat,col=ddf$colseg,add=T)
  }
  if (addActualPath){
    lines3d(ddf$orig.cas.erg,ddf$orig.cas.diat,ddf$orig.cas.iat,col="yellow",add=T)
  }
  
  # purple floor geometry
  p3 <- par3d()
  ergmin  <- p3$bbox[1]+10
  ergmax  <- p3$bbox[2]-10
  diatmin <- p3$bbox[3]+0.01
  diatmax <- p3$bbox[4]-0.01
  iatmin  <- p3$bbox[5]+0.1
  iatmax  <- p3$bbox[6]-0.1
  
  zipdf <- data.frame(x=c(ergmin,ergmin,ergmax,ergmax,ergmin),
                      y=c(0,0,0,0,0),
                      z=c(iatmin,iatmax,iatmax,iatmin,iatmin))
  polygon3d(zipdf$x,zipdf$y,zipdf$z,coord=c(1,3),alpha=0.5,color="purple",add=T)
}