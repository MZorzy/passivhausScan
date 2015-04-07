library(shiny)
source("globals.R")
                                        # options(error = recover);

static_links=list(`ProtoLife`="https://www.protolife.com",
    `Passivhaus`="http://www.passiv.de/en/index.php")
## #`Shiny`="http://shiny.rstudio.org")

##                                         # depends means the server restarts if one of these files is touched and
##                                         # the browser is refreshed. In Theory - but sometimes it seems like it's not working.
## lapply(Sys.glob(c("*.R","*.js","js/*.js")),
##        shiny:::dependsOnFile)

getfile = function(file){
    ret = readChar(file,file.info(file)$size)
    ret
}

shinyUI(
    tabsetPanel(id = 'tabs',
                tabPanel("PDT Passivhaus",
                         pageWithSidebar(
                             
                             headerPanel(tags$html(tags$img(src="/images/Circles0.75in.png" ,alt="xxx"),
                                                   tags$a(href="http://www.protolife.com","ProtoLife")),
                                         windowTitle="PDT"
#                                                   ,tags$html(HTML(getfile(welcomefile))))
                                         ),
                                        #,tags$a(href="http://www.protolife.com","ProtoLife")}
                             ##                                                  {tags$img(src="Circles0.75in.png")
                             ##                                                   tags$a(href="http://www.protolife.com","ProtoLife")},
                             ##                                                  windowTitle="PDT"),
                             sidebarPanel(
                                 htmlOutput('welcome'),
                                 h2("Variable exploration"),
                                 h3("Upload your Passivhaus file:"),
                                 fileInput('infile',"Input File")
                             ),
                             mainPanel(
                                 HTML(getfile('passivhausScanIntro.htm'))
                             )
                         )
                         ),
                tabPanel("Specify variable",
                         pageWithSidebar(
                             headerPanel(tags$html(tags$img(src="/images/Circles0.75in.png" ,alt="xxx"),
                                                   tags$a(href="http://www.protolife.com","ProtoLife"),tags$html(HTML(getfile('variable.htm')))),windowTitle="PDT"),
                             
                             sidebarPanel(
                                 h3("Variable specification"),
                                 uiOutput('choose_sheet'),
                                 textInput("cell","Cell (e.g. 'e20' or 'G13')"),
                                 textInput("desc","Description"),
                                 radioButtons("vtype",
                                              label = HTML("<h4>Range specification:</h4><p>choose from four different methods to specify a range:</p>"),
                                              choices = list("Min / Max / Number"=1,"Min / Max / Increment"=2,"Mid / Bound / Percentage"=3,"Explicit values"=4),selected=NULL),
                                 
                                 conditionalPanel(
                                     
                                     condition = "input.vtype == 1 & input.tabs=='Specify variable'",
                                     numericInput("min","Mininum",value=0.0),
                                     numericInput("max","Maximum",value=0.0),
                                     numericInput("num","Number of values",value=0.0)
                                 ),
                                 conditionalPanel(
                                     condition = "input.vtype == 2 & input.tabs=='Specify variable'",
                                     numericInput("min2","Mininum",value=0.0),
                                     numericInput("max2","Maximum",value=0.0),
                                     numericInput("inc","Increment",value=0.0)
                                 ),
                                 conditionalPanel(
                                     condition = "input.vtype == 3 & input.tabs=='Specify variable'",
                                     numericInput("mid","Midpoint",value=0.0),
                                     numericInput("percabove","Percent above",value=0.0),
                                     numericInput("percbelow","Percent below",value=0.0),
                                     numericInput("percinc","Percent increment",value=0.0)
                                 ),
                                 ## ############## NOTE:  if change number of inputs here, must change server.R code
                                 conditionalPanel(
                                     condition = "input.vtype == 4 & input.tabs=='Specify variable'",
                                     p("- leave zero's for unused values"),
                                     p("- don't forget to push Create Variable button below!"),
                                     numericInput("v1","Value 1",value=0.0),
                                     numericInput("v2","Value 2",value=0.0),
                                     numericInput("v3","Value 3",value=0.0),
                                     numericInput("v4","Value 4",value=0.0),
                                     numericInput("v5","Value 5",value=0.0),
                                     numericInput("v6","Value 6",value=0.0),
                                     numericInput("v7","Value 7",value=0.0),
                                     numericInput("v8","Value 8",value=0.0),
                                     numericInput("v9","Value 9",value=0.0),
                                     numericInput("v10","Value 10",value=0.0)
                                 ),
                                 actionButton("doit","Configure this variable"),
                                 HTML('<a href="#results">See results on the right...</a>')
                             ),
                             mainPanel(
                                 HTML('<a name="results"> </a>'), #??
                                 conditionalPanel(condition="input.doit == 0 & input.tabs=='Specify variable'",
                                                  h4("For reference, current default values of default variables:"),
                                                  p("Big picture:"),
                                                  tableOutput("bigpictureDefTable"),
                                                  p("Windows:"),
                                                  tableOutput("windowsDefTable"),
                                                  p("Assemblies:"),
                                                  tableOutput("assembliesDefTable")
                                                  ),
                                 conditionalPanel(
                                     condition="input.doit > 0 & input.tabs=='Specify variable'",
                                     h4("variable to be scanned:"),
                                     tableOutput("scanVarTable"),
                                     p("Please verify variable and range. If correct, hit the launch button to launch the scan:"),
                                     actionButton("launch","Launch",inline=TRUE)
                                 ),
                                 conditionalPanel(
                                     condition="input.launch > 0 & input.tabs=='Specify variable'",
                                     h4("Your variable scan has been launched!"),
                                     p("Below you will see a graph of results, momentarily.  Please note the progress indicator at the top of this window."),
                                     p("If a Passivhaus threshold is within the range, it is indicated with a horizontal dotted line."),
                                     h4("Sensitivities about default values"),
                                     plotOutput("PDTplot")                                       
                                 )
                             )
                         )
                         )
                )
)
