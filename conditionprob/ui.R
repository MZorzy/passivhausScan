library(shiny)
source("globals.R")
                                        # options(error = recover);

static_links=list(`ProtoLife`="https://www.protolife.com",
  `Passivhaus`="http://www.passivhaus.org/")
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
                    tabPanel("Passivhaus & PDT",
                             pageWithSidebar(
                                             
                                             headerPanel(tags$html(tags$img(src="/images/Circles0.75in.png" ,alt="xxx"),
                                                                   tags$a(href="http://www.protolife.com","ProtoLife")),windowTitle="PDT"),
                                        #,tags$a(href="http://www.protolife.com","ProtoLife")}
                                             ##                                                  {tags$img(src="Circles0.75in.png")
                                             ##                                                   tags$a(href="http://www.protolife.com","ProtoLife")},
                                             ##                                                  windowTitle="PDT"),
                                             sidebarPanel(
                                                          h2("Passivhaus with PDT"),
                                                          h3("Upload your Passivhaus file:"),
                                                          fileInput('infile',"Input File"),
                                                          Reduce(tagAppendChild,Map(
                                                                                    function(...)tags$li(tags$a(...,target="_blank"),tabindex="-1"),
                                                                                    names(static_links),href=static_links),
                                                                 tags$ul(class="nav nav-list",role="menu",`aria-labelledby`="dropdownMenu",
                                                                         tags$li(class="nav-header","External websites")))
                                                          ),
                                             mainPanel(
                                                       HTML(getfile('passivhausIntro.htm'))
                                                       )
                                             )
                             ),
                    tabPanel("Configure PDT variables",
                             pageWithSidebar(
                                             
                                             headerPanel(tags$html(tags$img(src="/images/Circles0.75in.png" ,alt="xxx"),
                                                                   tags$a(href="http://www.protolife.com","ProtoLife")),windowTitle="PDT"),
                                             sidebarPanel(
                                                          selectInput('addrm','Add or remove a variable?',choices=list('Add'=1,'Remove'=2),selected=0),
                                                          conditionalPanel(
                                                                           condition="input.addrm==1 & input.tabs = 'Configure PDT variables",

                                                                           h2("Add a variable:"),
                                                                           textInput("sheet","Sheet"),
                                                                           textInput("cell","Cell"),
                                                                           textInput("desc","Description"),
                                                                           radioButtons("vtype",
                                                                                        label = h3("Range specification:"),
                                                                                        choices = list("Min / Max / Number"=1,"Min / Max / Increment"=2,"Mid / Bound / Percentage"=3,"Explicit values"=4),selected=1),
                                                                           conditionalPanel(
                                                                                            condition = "input.vtype == 1 & input.tabs=='Configure PDT variables'",
                                                                                            numericInput("min","Mininum",value=0.0),
                                                                                            numericInput("max","Maximum",value=0.0),
                                                                                            numericInput("num","Number of values",value=0.0)
                                                                                            ),
                                                                           conditionalPanel(
                                                                                            condition = "input.vtype == 2 & input.tabs=='Configure PDT variables'",
                                                                                            numericInput("min2","Mininum",value=0.0),
                                                                                            numericInput("max2","Maximum",value=0.0),
                                                                                            numericInput("inc","Increment",value=0.0)
                                                                                            ),
                                                                           conditionalPanel(
                                                                                            condition = "input.vtype == 3 & input.tabs=='Configure PDT variables'",
                                                                                            numericInput("mid","Midpoint",value=0.0),
                                                                                            numericInput("percabove","Percent above",value=0.0),
                                                                                            numericInput("percbelow","Percent below",value=0.0),
                                                                                            numericInput("percinc","Percent increment",value=0.0)
                                                                                            ),
                                                                           ## ############## NOTE:  if change number of inputs here, must change server.R code
                                                                           conditionalPanel(
                                                                                            condition = "input.vtype == 4 & input.tabs=='Configure PDT variables'",
                                                                                            p("- leave zero's for unused values"),
                                                                                            p("- don't forget to push Create Variable button below!"),
                                                                                            ##                                                                for(i in 1:length(vnames)){
                                                                                            ##                                                                  numericInput(vnames[i],paste("Variable",vnames[i]),value = 0.0)
                                                                                            ##                                                                }
                                                                                            
                                                                                            numericInput("v1","Variable 1",value=0.0),
                                                                                            numericInput("v2","Variable 2",value=0.0),
                                                                                            numericInput("v3","Variable 3",value=0.0),
                                                                                            numericInput("v4","Variable 4",value=0.0),
                                                                                            numericInput("v5","Variable 5",value=0.0),
                                                                                            numericInput("v6","Variable 6",value=0.0),
                                                                                            numericInput("v7","Variable 7",value=0.0),
                                                                                            numericInput("v8","Variable 8",value=0.0),
                                                                                            numericInput("v9","Variable 9",value=0.0),
                                                                                            numericInput("v10","Variable 10",value=0.0)
                                        #                                                               numericInput("v11","Variable 11",value=0.0),
                                        #                                                               numericInput("v12","Variable 12",value=0.0)
                                                                                            ),
                                                                           actionButton("doit","Create variable"),
                                                                           HTML('<a href="#results">See results above...</a>')
                                                                           ), # end of conditional panel addrm=0 => add variable
                                                          conditionalPanel(
                                                                           condition="input.addrm==2 & input.tabs = 'Configure PDT variables",
                                        #                                 h3("________________________"),
                                                                           h2("Remove a variable:"),
                                        #                                 sliderInput("rmnum","Index of variable to remove",min=1,max=max(1,length(allVarList)),value=1),
                                                                           numericInput("rmnum","Index of variable to remove",value=0),
                                                                           actionButton("rmit","Remove variable")
                                                                           )
                                                          ),
                                             mainPanel(
                                                       HTML(getfile('variable.htm')),
                                                       HTML('<a name="results"> </a>'),
                                                       tableOutput("AllVarTable")
                                                       ##                                  conditionalPanel(
                                                       ##                                      condition = "input.addrm == 0 & input.tabs=='Specify variables'",
                                                       ##                                      HTML(getfile('variable.htm')),
                                                       ##                                      HTML('<a name="results"> </a>'),
                                                       ##                                      tableOutput("AllVarTable")
                                                       ##                                      ),
                                                       ##                                  conditionalPanel(
                                                       ##                                      condition = "input.addrm == 1 & input.tabs=='Specify variables'",
                                                       ##                                      HTML(getfile('variable.htm')),
                                                       ##                                      HTML('<a name="results"> </a>'),
                                                       ##                                      tableOutput("RmVarTable")
                                                       ##                                      )
                                                       )
                                             )
                             )
                    ## tabPanel("Remove Variable",
                    ##          pageWithSidebar(
                    
                    ##              headerPanel(tags$html(tags$img(src="/images/Circles0.75in.png" ,alt="xxx"),
                    ##                                    tags$a(href="http://www.protolife.com","ProtoLife")),windowTitle="PDT"),
                    ##              sidebarPanel(
                    ##                  h2("Passivhaus and PDT"),
                    ##                  Reduce(tagAppendChild,Map(
                    ##                      function(...)tags$li(tags$a(...,target="_blank"),tabindex="-1"),
                    ##                      names(static_links),href=static_links),
                    ##                         tags$ul(class="nav nav-list",role="menu",`aria-labelledby`="dropdownMenu",
                    ##                                 tags$li(class="nav-header","External websites")))
                    ##                  ),
                    ##              mainPanel(
                    ##                  h3("Variable to delete:")
                    ##                  )
                    ##              )
                    ##          ),
                    ## tabPanel("All Current Variables",
                    ##          pageWithSidebar(
                    
                    ##              headerPanel(tags$html(tags$img(src="/images/Circles0.75in.png" ,alt="xxx"),
                    ##                                    tags$a(href="http://www.protolife.com","ProtoLife")),windowTitle="PDT"),
                    ##              sidebarPanel(
                    ##                  h2("Passivhaus and PDT"),
                    ##                  Reduce(tagAppendChild,Map(
                    ##                      function(...)tags$li(tags$a(...,target="_blank"),tabindex="-1"),
                    ##                      names(static_links),href=static_links),
                    ##                         tags$ul(class="nav nav-list",role="menu",`aria-labelledby`="dropdownMenu",
                    ##                                 tags$li(class="nav-header","External websites")))
                    ##                  ),
                    ##              mainPanel(
                    ##                  h3("All variables in the experimental space:"),
                    ##                  tableOutput("AllVarTable")
                    ##                  )
                    ##              )
                    ##          )
                    )
        )
