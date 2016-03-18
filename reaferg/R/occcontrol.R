occcontrol <- function (date,occpred,preheat){
  hh <- hour(date)
  mi <- minute(date)
  hhf <- hh + mi/60
  lolim <- preheat$start-preheat$hours
  hilim <- preheat$start
  override <- lolim<hhf & hhf<=hilim
  #   if (override){
  #     print(sprintf("hh:%d mi:%d hhf:%.2f   ph.hours:%.2f lolim:%.2f hilim:%.2f  ovr:%s",hh,mi,hhf,preheat$hours,lolim,hilim,override))
  #   }
  
  occval <- ifelse( override,preheat$level,occpred )
  rv <- list()
  rv$occval <- occval
  rv$override <- override
  return(rv)
}