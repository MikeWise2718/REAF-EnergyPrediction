# server.r
# SimpleShinyInput Notes: this is a basic simple Shiny program with a few input controls. 
?a
library(reaferg)
library(lubridate,quietly=T,warn.conflicts=F)
library(randomForest,quietly=T,warn.conflicts=F)
library(scales,quietly=T,warn.conflicts=F)
library(ggplot2,quietly=T,warn.conflicts=F)
library(grid,quietly=T,warn.conflicts=F)
library(gridExtra,quietly=T,warn.conflicts=F)

log <- list()

addLog <- function (s){
  log[[length(log)+1]] <<- s
}


ver <- initVer()
epath <- getErgPaths(bpath="c:/transfer/deshark/")
rparms <- getRparms("City Center")
rparms$parms <- data.frame(ver$starttime)
feats <- getFeatures()
rparms$feats <- feats
rparms$epath <- epath

enableRetrain <- TRUE

set.seed(1234)

shinyServer(function(input, output,session) 
{
  updateDateRangeInput(session, "traindaterange",
                       label = "Date range:",
                       start = as.character(rparms$stime),
                       end = as.character(rparms$etime))
  
  rparms$ptime <- rparms$etime - 24*3600
  
  updateDateInput(session, "predictdate",
                  label = "Predict Date:",
                  value = as.character(rparms$ptime),
                  min = as.character(rparms$stime),
                  max = as.character(rparms$ptime))


  addLog(paste0("Start of Log at ",Sys.time()))
  
  # Training Inputs

  
  newbld <- reactive({
    rparms$bld <<- input$bld
  })
  
  
  trainingInputChanges <- reactive({
    rparms$bld <<- input$bld
    rparms$trainfrac <<- input$trnpct/100
    #print(rparms$trainfrac)
    rparms$rfntree <<- input$rfntree
    #print(input$rfntree)
    #addLog(input$rfntree)
    rparms$runname <<- input$runname
    rparms$dispvar <<- input$dispvar
    rparms$ergvar <<- input$ergvar
    rparms$nchg <<- rparms$nchg + 1
    rparms$stime <<- as.POSIXct(input$traindaterange[1],tz=rparms$tzab)
    rparms$etime <<- as.POSIXct(input$traindaterange[2],tz=rparms$tzab)
    rparms$ptime <<- rparms$etime - 24*3600
    updateDateInput(session, "predictdate",
                    label = "Predict Date:",
                    value =as.character(rparms$ptime),
                    min = as.character(rparms$stime),
                    max = as.character(rparms$ptime))
    addLog(sprintf("Training Params Changed %d",rparms$nchg))
    addLog(as.POSIXct(input$traindaterange[1],tz=rparms$tzab))
    return(rparms$nchg)
  })
  newtrained <- reactive(
    {
      input$retrain
      print(sprintf("Retrain %d of %s",rparms$traincount,rparms$bld))
      print(sprintf("   dates from %s to %s  trainfrac:%.2f trees:%d at %s",
                     as.character(rparms$stime),
                     as.character(rparms$etime),rparms$trainfrac,rparms$rfntree,Sys.time()))
      tset <- trainAll(rparms)

      rparms$traincount <<- rparms$traincount+1


      print(sprintf("tsetlen:%d",length(tset)))
      return(tset)
    })
  newpredict <- reactive(
    {
      input$predict
      addLog("Pushed predict")
      tset <- newtrained()
      if (!is.null(tset)){
        if (rparms$occpreheat){
          pset <- runSenarios(rparms,rparms$stophr,rparms$hrs,rparms$lvs,
                              tset$occ,tset$erg,tset$diat,tset$iat)
        } else {
          pset <- runSenarios(rparms,rparms$stophr,0,0,
                              tset$occ,tset$erg,tset$diat,tset$iat)
        }
      }
      return(pset)
    })
  
  
  
# Training Outputs  
  output$bld = renderText({rparms$bld})
  output$stime = renderText({"2013-01-01"})
  output$etime = renderText({"2014-01-01"})

  output$echo = renderPrint(
    {
      ntchg <- trainingInputChanges()
      npchg <- predictInputChanges()
      for (s in log){
        cat(sprintf("%s\n",s))
      }
    }
  )
  output$dataframe <- renderDataTable({
    pset <- newpredict()
    pset$casdf
#    data.frame(x=rnorm(100),y=rnorm(100))
  },
  options = list(
    pageLength = 100
  )
  )
  output$r2plot = renderPlot(
    { 
      tset <- newtrained()
      return(tset[[input$dispvar]]$r2plot)
    }
  )

#  output$enableRetrain = enableRetrain

  output$impplot = renderPlot(
    { 
      tset <- newtrained()
      return(grid.draw(tset[[input$dispvar]]$impplot))
    }
  )
  output$overviewplot = renderPlot(
    { 
      tset <- newtrained()
      return(tset[[input$dispvar]]$overviewplot)
    }
  )
  output$detailplot = renderPlot(
    { 
      tset <- newtrained()
      return(tset[[input$dispvar]]$detailplot)
    }
  )

# Prediction Inputs
  
  predictInputChanges <- reactive({
    cdt <- as.character(input$predictdate)
    st <- as.POSIXct(cdt,tz=rparms$tzab)
#    hour(st) <- 0
#    minute(st) <- 0
#    second(st) <- 0
    et <- st
    hour(et) <- 23
    minute(et) <- 45
    
    rparms$sitime <<- st
    rparms$eitime <<- et
    
    addLog("Predict Params Changed")
    fmt <- date_format("%y-%m-%d %R %Z",tz=rparms$tzab)
    addLog(sprintf(" st:%s et:%s %s",fmt(st),fmt(et),rparms$tzab))
    
    shr <- input$occphhour
    hrgran <- as.numeric(input$occphhourgran) 
    rparms$hrs <<- seq(shr[1],shr[2],by=hrgran)
    
    #addLog(rparms$hrs)
    slv <- input$occlev
    lvsteps <- as.numeric(input$occlevsteps)
    rparms$lvs <<- seq(slv[1],slv[2],length.out=lvsteps)
    #addLog(rparms$lvs)
    rparms$stophr <<- as.numeric(input$stoptime)
    #addLog(rparms$stophr)
    rparms$occpreheat <<- input$occpreheat
    return(0)
  })
  
# Predictive Outputs  
  output$predictplot = renderPlot(
    { 
      pset <- newpredict()
      return(pset$plts[[input$disppredvar]])
    }
  )
}
)