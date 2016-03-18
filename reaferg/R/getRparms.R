getRparms <- function(blld,rseed=1234){
  rparms <- list()
  rparms$bld <- blld
  rparms$bchtime <- "---bchtime---"
  rparms$runname <- "devel"
  rparms$gwidth <- 12
  rparms$gheight <- 9
  rparms$randomseed <- rseed
  set.seed(rseed)
  
  rparms$trainfrac <- 0.93
  rparms$rfntree <- 70
  
  rparms$ergname <- "e.tot"
  
  rparms$noiselev <- 9
  
  rparms$usagecostkwh <- (0.06209+0.016377)
  rparms$demandcostkw <- 3.78
  
  rparms$tzab <- "US/Pacific"
  rparms$pltstime <- ymd_hms("2013-01-01 00:00:00",tz=rparms$tzab)
  rparms$stime <- ymd_hms("2014-01-01 00:00:00",tz=rparms$tzab)
  rparms$stime <- ymd_hms("2014-10-01 00:00:00",tz=rparms$tzab)
  rparms$etime <- ymd_hms("2014-12-03 23:45:00",tz=rparms$tzab)
  
  rparms$sitime <- rparms$stime
  rparms$eitime <- rparms$etime
  rparms$sitime <- ymd_hms("2014-11-18 00:00:00",tz=rparms$tzab)
  rparms$eitime <- ymd_hms("2014-11-18 23:45:00",tz=rparms$tzab)
  
  rparms$hvaclevs <- c("heating","drift","cooling")
  rparms$hvaccolors <- c("red","green","blue")
  
  # shiny specific
  rparms$trainingInvalid <- T
  rparms$traincount <- 0
  rparms$predictcount <- 0
  rparms$nchg <- 0
  
  rparms$dispvar <- "occ"
  rparms$ergvar <- "e.tot"
  
  return(rparms)
}