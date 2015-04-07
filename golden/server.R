
library(shiny)

options(shiny.maxRequestSize=30*1024^2)
source("globals.R")

# which inputs are synched and saved via the URL
url_fields_to_sync <- 
  c("tabSelected")
#  c("dataSet","tabSelected")

# Note that permission was not sought to use the following link -
# use with caution or provide your own.
onlineHelpServer="http://finzi.psych.upenn.edu/R/library/"

# to use minimised js, run the uglify script (edit as necessary for paths)
# then change below to ./js_min
#addResourcePath("js",tools:::file_path_as_absolute("./js")) 

# Define server logic required to summarize and view the selected dataset

tstcnt = 1
getvarlist =  function(input){
    if(is.null(input$infile))
        return("Warning:  before defining variables you must upload your Passivhaus xls file!")
    rtn = list()
    gotit = 0
    if(input$vtype == 1){
        mn = input$min
        mx = input$max
        num = input$num
        vals = numeric(num)
        inc = (mx-mn)/(num-1)
        for(i in 1:num)
            vals[i] = mn + (i-1)*inc
        gotit  = 1
    }
    else if(input$vtype == 2){
        mn = input$min2
        mx = input$max2
        inc = input$inc
        num = 1+(mx - mn)/inc
        if (num-round(num,0) == 0){
            vals = numeric(num)
            for(i in 1:num)
                vals[i] = mn + (i-1)*inc
        }
        else{                               # got a bit at the end...
            num = num+1
            vals = numeric(num)
            for(i in 1:(num-1))
                vals[i] = mn + (i-1)*inc
            vals[num] = mx
        }
        gotit  = 1
    }
    else if(input$vtype == 3){
        mn = input$mid  * (1.0-input$percbelow / 100.0)
        mx = input$mid * (1.0 + input$percabove / 100.0)
        inc = input$mid * (input$percinc / 100)
        num = 1+ (mx - mn)/inc                 # now same as vtype == 2
        if (num-round(num,0) == 0){
            vals = numeric(num)
            for(i in 1:num)
                vals[i] = mn + (i-1)*inc
        }
        else{                               # got a bit at the end...
            num = num+1
            vals = numeric(num)
            for(i in 1:(num-1))
                vals[i] = mn + (i-1)*inc
            vals[num] = mx
        }
        gotit  = 1
    }
    else if(input$vtype == 4){            ####### NOTE  names and number of values must match ui.R code
        num = length(vnames)
        vals = numeric(0)
        for(i in 1:length(vnames)){
            vv = input[[vnames[i]]]
            if(vv!=0)
                vals[i] = vv
            else
                break
        }
        num = length(vals)
        gotit  = 1
    }
    if(gotit){
        rtn$sheet = input$sheet
        rtn$cell = input$cell
        rtn$desc = input$desc
        vnm = paste('v',1:num,sep='')
        mn = length(rtn)
        for(i in 1:num)
            rtn[vnm[i]] = vals[i]
        if(rtn$sheet=="" || rtn$cell == "")
            rtn = "Sheet and cell must be specified!"
    }
    else
        rtn="getvarlist:  input$vtype not an appropriate value..."
    if(is.list(rtn)){
            allVarList[[length(allVarList)+1]] <<- rtn
        }

    rtn
}

getallvar = function(){
    if(length(allVarList)==0)
        return(data.frame(Warning="No variables added yet!"))
    nc = 0
    for(i in 1:length(allVarList)){
        len = length(allVarList[[i]])
        if(len>nc){
            nc = len
            idx = i
        }
    }
    nr = length(allVarList)
    nm = names(allVarList[[idx]])       # names of longest list item
    rtn = matrix(nrow=nr, ncol=nc)      # matrix filled with NA
    for(i in 1:nr){
        ll = length(allVarList[[i]])
        for(j in 1:ll)
            rtn[i,j] = allVarList[[i]][[j]]
    }
    colnames(rtn) = nm
    print(rtn)
    for(i in 4:ncol(rtn)){
        for(j in 1:nrow(rtn)){
            val = rtn[j,i]
            if(!is.na(rtn[j,i]))
                rtn[j,i] = round(as.numeric(val),2)
        }
    }
    rtn = data.frame(rtn)
    rtn
}

rmvarlist=function(input){
    rmnum = input$rmnum
    if(rmnum<1 || rmnum> length(allVarList))
        return
#    allVarList[[length(allVarList)+1]] = "test mssg from rmvarlist()"
    allVarList <<- allVarList[-rmnum]
}

restart_ = -1
run_ = 0

ndoit = 0
nrmit = 0
odoit = 0
ormit = 0

shinyServer(function(input, output,session) {
    observe({                           
        input$doit                      # Create button pushed
        updateSelectInput(session,'addrm',choices=list('add'=0,'remove'=1),selected=0) # to control plot using output$AllVarTable
        dat = isolate(getvarlist(input))
    })
    observe({                           
        input$rmit                      # Remove button pushed
        updateSelectInput(session,'addrm',choices=list('add'=0,'remove'=1),selected=1) # plot with output$RmVarPlot
        dat = isolate(rmvarlist(input))
    })
                              
    output$RmVarTable = renderTable({   # output for removed variable
#        isolate(rmvarlist(input)) # removes the variable indicated by input$rmnum
        dat = getallvar()               # allVarList should have variable removed...
    })        
    output$AllVarTable = renderTable({  # output for added variable
        ## input$doit
        ## input$rmit
        ## dat = isolate(getvarlist(input)) # just to compute the new variable and install it...
        ## dat = getallvar()                # data frame with all variables to display.
        ## dat
      input$doit
      input$rmit
      dat = getallvar()
      dat = data.frame(dat)
      dat
    })
  })

