getFeatures <- function(){
  feats <- list()
  feats$time <- c("time","year","ydayf","hourf","wday","holi")
  feats$bld <- c("hdcnt")
  feats$wetr <- c("temp.f","tcool.f","theat.f","dewp.f","pres.pa","wind.deg","wind.kmh","hum.pc","rain.mm")
  feats$pred <- c("occ")
  feats$dwetr <- c("tcool.f","theat.f","dewp.f","pres.pa","wind.deg","wind.kmh","hum.pc","rain.mm")
  return(feats)
}