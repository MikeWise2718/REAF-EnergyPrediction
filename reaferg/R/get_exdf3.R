get_exdf3 <- function (rparms,rdf,slmfit,rffit) 
{
  erggran <- 100
  if (mean(rdf$erg) < 1000) {
    erggran <- 50
  }
  iatgran <- 0.5
  tpfgran <- 5
  quanlim <- c(0.02, 0.98)
  erglim <- erggran * trunc(quantile(rdf$erg, quanlim)/erggran)
  iatlim <- iatgran * trunc(quantile(rdf$iat, quanlim)/iatgran)
  tpflim <- tpfgran * trunc(quantile(rdf$temp.f, quanlim)/tpfgran)
  ergvals <- seq(erglim[[1]], erglim[[2]], erggran)
  iatvals <- seq(iatlim[[1]], iatlim[[2]], iatgran)
  tpfvals <- seq(tpflim[[1]], tpflim[[2]], tpfgran)
  exdf <- expand.grid(erg = ergvals, iat = iatvals, temp.f = tpfvals)
  hvaccolors <- rparms$hvaccolors
  names(hvaccolors) <- rparms$hvaclevs
  if (!is.null(slmfit)){
    pmdf <- predict(slmfit, exdf, trace = T, type = "dataframe")
    pmdf$pred.lm.diat <- pmdf$diat
    pmdf$diat <- NULL
    exdf <- merge(pmdf, exdf, c("erg", "iat", "temp.f"))
  }
  exdf$pred.diat <- predict(rffit, exdf)
  exdf$segname <- factor(exdf$segname, rparms$hvaclevs)
  exdf$iat.minus.oat <- exdf$iat - exdf$temp.f
  exdf$mtemp <- -exdf$temp.f
  exdf$ftemp <- as.factor(exdf$temp.f)
  exdf$ftemp <- factor(exdf$ftemp, levels = rev(levels(exdf$ftemp)))
  exdf$fiat <- as.factor(exdf$iat)
  exdf$fiat <- factor(exdf$fiat, levels = rev(levels(exdf$fiat)))
  exdf$imo <- trunc((exdf$iat.minus.oat + 5)/10) * 10
  return(exdf)
}