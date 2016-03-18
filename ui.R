# ui.R
# SimpleShinyInput Notes: this is a basic simple Shiny program with a few input devices. 
datatabstyle <- "font-size:80%;font-family:'Lucida Console New'"

shinyUI(fluidPage(
  fluidRow(
    column(3,img(src="shark-ss-l.png")),
    column(6,titlePanel("RE&F Building De-sharking")),
    column(3,img(src="shark-ss-r.png"))
  ),
  sidebarLayout(position = "left",
            sidebarPanel( 
              column(12,style = "height:600px;background-color: #ffffff;",
                  fluidRow(
                  tabsetPanel(
                    tabPanel("Train",
                             fluidRow(
                               column(4,h4("Training")),
                               column(4,actionButton("retrain", "Retrain")),
                               column(4,checkboxInput("retrainEnable", label = "Enable",value=TRUE))
                             ),
                             fluidRow(
                               column(6,selectInput("bld", "Building:",c("City Center","REDW A","REDW B","B99","WILLOWS 10525"))),
                               column(6,selectInput("dispvar", "Display Var",c("occ","diat","iat","erg"),"erg"))
                             ),
                             dateRangeInput("traindaterange", "Date range:",
                                            start = textOutput("stime"),
                                            end   = textOutput("etime"),
                                            weekstart=1),
                             fluidRow(
                               column(6,sliderInput("trnpct","Train Pct:",10,100,70,5)),
                               column(6,sliderInput("rfntree","RfTrees:",0,150,10,5))
                             )
                    ),
                    tabPanel("Predict",
                             fluidRow(
                               column(6,h4("Prediction")),
                               column(6,actionButton("predict", "Repredict"))
                             ),
                             fluidRow(
                               column(6,dateInput("predictdate", "Predict Date:", format = "yyyy-mm-dd", 
                                       startview = "month", weekstart = 1, value="2014-01-01 PST", language = "en", width = NULL)),
                               column(6,checkboxInput("occpreheat", label = "OCC Pre-heat",value=FALSE))
                             ),
                             fluidRow(
                               column(6,selectInput("stoptime", "Stop Time",c("7:00"=7.0,"7:15"=7.25,"7:30"=7.5,"7:45"=7.75,"8:00"=8,"8:15"=8.25,"8:30"=8.5))),
                               column(6,selectInput("disppredvar", "Display Var",c("occ","diat","iat","erg"),"erg"))
                             ),
                             fluidRow(
                               column(8,sliderInput("occlev","OCC Lev:",0,1,c(0.1,0.9),0.1)),
                               column(4,selectInput("occlevsteps", "Steps",c(1,2,3,4,5),3))
                             ),
                             fluidRow(
                               column(8,sliderInput("occphhour","Pre-heat hours:",0,6,c(2,4),0.25)),
                               column(4,selectInput("occphhourgran", "Granularity",c(2,1,0.5,0.25),1))
                             )
                    )
                  ),
                  tabPanel("Log", verbatimTextOutput('echo'))
                ))),
                mainPanel(
                  column(12,style = "height:700px;background-color: #c0b4ee;",
                         tabsetPanel(selected="Blank",
                                     tabPanel("Blank",h3("Switch tab to start")),
                                     tabPanel("R2",h3("R2 Training Convergence"),plotOutput("r2plot",height="600px")),
                                     tabPanel("Imp",h3("Variable Importance"),plotOutput("impplot")),
                                     tabPanel("Overview", plotOutput("overviewplot",height="600px")),
                                     tabPanel("Detail", plotOutput("detailplot",height="600px")),
                                     tabPanel("Prediction", plotOutput("predictplot",height="600px")),
                                     tabPanel("Data", h3("Data Dump"),
                                              div(dataTableOutput("dataframe"), class="table", style =datatabstyle ))
                         )
                  )
                )
  )
))