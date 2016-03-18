runSenarios <- function(rparms,starthr,hrs,lvs,occlst,erglst,diatlst,iiatlst){

  fmt <- date_format("%y-%m-%d %R %Z")
  print(sprintf("runScenarios - %s sitime:%s  eitime:%s",rparms$bld,fmt(rparms$sitime),fmt(rparms$eitime)))
  
  adf <- NULL
  cdf <- NULL
  tdf <- NULL
  
  mdf <- mergeDfsByQhr(occlst$rdf,erglst$rdf,iiatlst$rdf)
  
  preheat <- list()
  preheat$start <- starthr
  preheat$hours <- 4
  preheat$level <- 0.5
  
  for (hour in hrs){
    for (level in lvs){
      preheat$hours <- hour
      preheat$level <- level
      crv <- cascadeModels(rparms,occlst$rf,erglst$rf,diatlst$rf,iiatlst$rf,mdf,
                          diatlst$iatlims,preheat=preheat)
      adf <- combineData(adf,crv)
      cdf <- combineCosts(cdf,crv)
    }
  }
  rv <- list()
  rv$casdf <- crv$casdf
  rv$cdf <- cdf
  rv$adf <- adf
  rv$odf <- occCdfTotdf(cdf)
  rv$edf <- ergCdfTotdf(cdf)
  rv$tdf <- iatCdfTotdf(cdf)
  rv$plts <- doPlots(rv$adf,rv$odf,rv$edf,rv$tdf,rparms,"rf")
  return(rv)
}