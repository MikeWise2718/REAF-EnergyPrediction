cascadeModels <- function(rparms,occrf,ergrf,diatrf,iatrf,indf,iatlims=NULL,trace=F,preheat){
  sstime <- rparms$sitime
  eetime <- rparms$eitime
  
  # 
  indf$occ <- as.numeric(as.character(indf$occ))
  idx <- sstime<=indf$time & indf$time<=eetime
  if (sum(idx)>0){
    casdf <- indf[ idx, ]
  } else {
    return(NULL)
  }
  #  casdf[["pred1.occ"]] <- predict(occrf,casdf)
  #  casdf[["pred1.erg"]] <- predict(ergrf,casdf)
  #  casdf[["pred1.diat"]] <- predict(diatrf,casdf)
  casdf[["pred.cas.occ"]] <- NA 
  casdf[["pred.cas.erg"]] <- NA 
  casdf[["pred.cas.diat"]] <- NA 
  casdf[["pred.cas.iat"]] <- NA 
  casdf[["pred.iat"]] <- NA
  if (!is.null(iatlims)){
    casdf[["iatseg"]] <- "heating"
    #print(sprintf("iatlims:%.1f %.1f",iatlims[1],iatlims[2]))
  }
  casdf[["orig.cas.occ"]] <- casdf[["occ"]]
  casdf[["orig.cas.erg"]] <- casdf[["erg"]]
  casdf[["orig.cas.diat"]] <- casdf[["diat"]]
  casdf[["orig.cas.iat"]] <- casdf[["iat"]]
  casdf[1,"pred.iat"] <- casdf[1,"iat"]
  iend <- nrow(casdf)
  occovcnt <- 0
  for( i in 2:iend ){
    predoccval  <- as.numeric(as.character(predict(occrf,casdf[i-1,])))
    occctrl <- occcontrol(casdf[i,"time"],predoccval,preheat)
    casdf[i,"pred.cas.occ"] <- occcontrol(casdf[i,"time"],predoccval,preheat)$occval
    if (occctrl$override){
      occovcnt <- occovcnt+1
    }
    
    #  casdf[i,"pred.cas.occ"]  <- as.numeric(casdf[i,"occ"])   # Add this for an OCC Lock
    
    casdf[i,"occ"] <- as.numeric(as.character(casdf[[i,"pred.cas.occ"]]))
    #  browser()
    casdf[i,"pred.cas.erg"]  <- predict(ergrf,casdf[i-1,])
    
    # casdf[i,"pred.cas.erg"]  <- casdf[i,"erg"]   # Add this for an ERG Lock
    casdf[i,"erg"] <- casdf[i,"pred.cas.erg"]   
    casdf[i,"pred.cas.diat"] <- predict(diatrf,casdf[i-1,])
    casdf[i,"diat"] <- casdf[i,"pred.cas.diat"]
    casdf[i-1,"liat"] <- casdf[max(1,i-2),"iat"]
    # casdf[i-1,"diat"] <- casdf[max(1,i-2),"iat"]-casdf[max(1,i-3),"iat"]
    casdf[i,"pred.new.iat"] <- predict(iatrf,casdf[i-1,])
    if (i<iend){
      casdf[i,"pred.cas.iat"] <- max(64,min(80,casdf[i-1,"iat"] + casdf[i,"diat"]))
      casdf[i,"iat"] <- casdf[i,"pred.cas.iat"]
      casdf[i,"iat"] <- casdf[i,"pred.new.iat"]
      #      casdf[i+1,"iat"] <- casdf[i,"iat"] + casdf[i,"diat"]
      if (!is.null(iatlims)){
        if (casdf[i,"pred.cas.iat"]>iatlims[2]){
          casdf[i,"iatseg"] <- "cooling"
        } else if(casdf[i,"pred.cas.iat"]>iatlims[1]){
          casdf[i,"iatseg"] <- "drift"
        } 
      }
    }
    casdf[i,"pred.iat"] <- casdf[i-1,"pred.iat"] + casdf[i,"orig.cas.diat"]
    
    
    
    if(trace){
      msg <- sprintf("t:%d - pred.cas.iat_t:%.1f orig.cas.iat_t:%.1f erg_t:%.1f oat_t:%.1f ->  diat_t:%.2f iat.new_t:%.2f %s",
                     i,casdf[i,"pred.cas.iat"],casdf[i,"orig.cas.iat"],
                     casdf[i,"erg"],casdf[i,"temp.f"], casdf[i,"diat"],
                     casdf[i,"pred.new.iat"],
                     casdf[i,"iatseg"]
      )
      print(msg)
    }
  }
  
  #  rparms$usagecostkwh <- 0.06
  #  rparms$demandcostkw <- 6.00  
  
  
  peakerg <- max(casdf[["pred.cas.erg"]],na.rm=T )
  sumerg  <- sum(casdf[["pred.cas.erg"]],na.rm=T)/4
  avgiat <- mean(casdf[["pred.cas.iat"]],na.rm=T)
  avgocc <- mean(casdf[["pred.cas.occ"]],na.rm=T)
  dc <- peakerg*rparms$demandcostkw
  uc <- sumerg*rparms$usagecostkwh
  tc <- dc+uc
  
  msg <- sprintf("shr:%.1f hr:%.2f lv:%.2f peakE:%.1f sumE:%.1f dc:%.2f uc:%.2f tc:%.2f occov:%d avgT:%.3f",
                 preheat$start,preheat$hours,preheat$level,
                 peakerg,sumerg,dc,uc,tc,
                 occovcnt,avgiat)
  print(msg)
  
  rv <- list()
  rv$casdf <- casdf
  rv$peakerg <- peakerg
  rv$sumerg <- sumerg
  rv$dc <- dc
  rv$uc <-  uc
  rv$tc <-  tc
  rv$avgocc <- avgocc
  rv$avgiat <- avgiat
  rv$preheat <- preheat
  rv$occovcnt <- occovcnt
  rv$sstime <- sstime
  rv$eetime <- eetime
  return(rv)
}