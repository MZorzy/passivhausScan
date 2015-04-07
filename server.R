

library(shiny)

options(shiny.maxRequestSize=30*1024^2)
source("globals.R")
source("excel.R")
source("db.R")
options(shiny.trace=T)

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
#getVarFiles = function(){
#    ret = system('cd VarFiles; ls',intern=T)
#    if(length(ret)==0)
#        ret = 'No variable files found'
#    ret
#}
#varfiles <<- getVarFiles()

firstfoo = 1
                                   
mkallVarList = function(){
    allVarList <<- c(bigpictureVarList,windowsVarList,assembliesVarList)
}
                                   
## initialize variables to the defaults...
loadVars = function(varfile = 'bigpictureDefaultVariables'){
  logfoo(paste('Trying to load varfile ',varfile))
  logfoo(paste('length of allVarList before = ',length(allVarList)))

  if(length(varfile)==0)
      return(NULL)
  if(is.na(varfile))
      return(NULL)
  if(varfile == "" || is.null(varfile))
      return(NULL)
  tmpfile = paste(getwd(),'/output/tmp',rndtag,'.RData',sep='')
  system(paste('cp ',varfile,tmpfile))
  attach(tmpfile)       # should be bigpictureVarList, windowsVarList or assembliesVarList
  nms = c('bigpictureVarList', 'windowsVarList','assembliesVarList')
  for(xx in ls(pos=2)){
      if(length(grep(xx,nms)) > 0)      #good name
          assign(xx,get(xx,pos=2),pos=1)
      else
          logfoo(paste('bad loaded variable: ',xx))
  }
  detach(2)
  mkallVarList()
  logfoo(paste('length of allVarList after = ',length(allVarList)))
}

initialize = function(){                   #load default files
    warning(paste('----------starting---------',Sys.time()))
    rndtag <<- round(10000*abs(runif(1)),0)
    logfile <<- paste('output/log',rndtag,sep='')

    system(paste('rm -rf ',logfile))
    loadVars('bigpictureDefaultVariables')
    loadVars('windowsDefaultVariables')
    loadVars('assembliesDefaultVariables')
    mkallVarList()
    stopifnot(db_init()==TRUE)
}


## This function is called when new variables are created.
## The new variable is created from one of three methods of specifying a range of values
## Then validation tests with spreadsheet are performed
## if all tests passed, new variable is constructed

getvarlist =  function(input){
    if(is.null(input$infile))
        return("Warning:  before defining variables you must upload your Passivhaus xls file!")
    rtn = list()
    myVarList = list()
    gotit = 0
    logfoo("==== entered getvarlist")
    if(input$vtype == 1){               # min / max / number
        mn = input$min
        mx = input$max
        num = input$num
        vals = numeric(num)
        inc = (mx-mn)/(num-1)
        for(i in 1:num)
            vals[i] = mn + (i-1)*inc
        gotit  = 1
    }
    else if(input$vtype == 2){          # min / max / increment
        mn = input$min2
        mx = input$max2
        inc = input$inc
        num = 1+(mx - mn)/inc
        logfoo(paste('min/max/inc , num:',mn,mx,inc,num))
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
    else if(input$vtype == 3){          # mid / bound / percentage
        mn = input$mid  * (1.0-input$percbelow / 100.0)
        mx = input$mid * (1.0 + input$percabove / 100.0)
        inc = input$mid * (input$percinc / 100)
        num = 1+ (mx - mn)/inc                 # now same as vtype == 2
        logfoo(paste('mid/bound/percentage , num:',mn,mx,inc,num))
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
            if(length(vv)==0)
                break
            if(vv!=0)
                vals[i] = vv
            else
                break
        }
        num = length(vals)
        gotit  = 1
    }
######################## finished creating values!
######################## Now check for errors and create new entry to allVarList

######################## check to remove error msg from allVarList and myVarList
    ## idx = 0
    ## if(length(allVarList)>0){
    ##     for(i in 1:ln)
    ##         if(length(allVarList[[i]])==1){    # an error message
    ##             allVarList <<- allVarList[-i-idx]
    ##             idx = idx+1                     # in case more than one
    ##         }
    ## if(length(myVarList)>0)
    ##     for(i in 1:length(myVarList))
    ##         if(length(myVar[[i]])==1){    # an error message
    ##             myVarList = myVarList[-i-idx]
    ##             idx = idx+1                     # in case more than one
    ##         }
#################### start checks for errors
    gotdup  = 0
    cellcheckres = ""
    logfoo(paste('======getallvar:  gotit =',gotit))
    for(i in 1:length(vals))
        logfoo(paste('======getallvar:  vals[',i,'] = ',vals[i]))
    if(gotit){                          
        sheet = input$sheet
        cell = input$cell
############# # check for duplicate sheet,cell         
##         if(length(myVarList)>0)
##             for(i in 1:length(allVarList)){  
##                 if(length(myVarList[[i]])==1)    # an error message in myVarList
##                     next
##                 ll = myVarList[[i]]
##                 nm = names(ll)
##                 if('sheet' %in% nm && 'cell' %in% nm){
##                     if(ll$sheet ==sheet && ll$cell == cell)
##                         gotdup = 1
##                 }
##             }
############## end of check for dup      
############### now check for cell value
        logfoo(paste('======getallvar:  1'))
        mn=min(vals)                        
        mx = max(vals)
        chk = checkcell(sheet,cell,mn,mx)
        logfoo(paste('======getallvar:  2'))

############### end check for cell value
############### check for error conditions
        if(sheet=="" || cell == "")
            rtn = list("Sheet and cell must be specified!")
        else if(gotdup){
            logfoo(paste('======getallvar:  gotdup'))
            rtn = list("Duplicate sheet,cell...")
        }
        else if(is.character(chk)){               # got error from checkcell
            logfoo(paste('======getallvar:  checkcell'))
            rtn = switch(chk,
                'noload'=list("Workbook not loaded"),
                'badsheet' = list("Specified sheet not found"),
                'badrow' = list("Specified row not found"),
                'missingval' = list("Shouldn't get missingval error..."),
                'outofrange' = list("Cell value outside specified range"),
                chkcell
                )
        }
###################### no errors found!!!!!!!!!
        else{                          # so construct the good rtn value
            logfoo(paste('======getallvar:  constructing return'))            
            rtn$sheet = sheet
            rtn$cell = cell
            rtn$desc = input$desc
            if(chk == TRUE)
                tmp = ''
            else if(chk == FALSE)
                tmp = '*'
            else
                tmp = 'X'
#            rtn$numeric = tmp
            vnm = paste('v',1:num,sep='')
            logfoo(paste('======getallvar:  constructing return vals'))                        
            for(i in 1:num)
                rtn[vnm[i]] = vals[i]
        }
    }
    else
        rtn="getvarlist:  input$vtype not an appropriate value..."
    rtn
  }

## ###################### add rtn to the appropriate VarList
##    logfoo(paste('===== +++ len rtn = ',length(rtn)))
##    warning(paste('===== +++ len rtn = ',length(rtn)))
##    if(is.list(rtn)){
##       allVarList[[length(allVarList)+1]] <<- rtn
##       warning(paste('===== +++ len allVarList = ',length(allVarList)))
##       logfoo(paste('===== +++ len allVarList = ',length(allVarList)))
##     }
##     else
##         logfoo(paste('============ bad rtn:',rtn))
##     rtn
## }

getallvar = function(){
    if(length(allVarList)==0)
        return(data.frame(Warning="No variables added yet!"))
    else if(length(allVarList[[1]] )==1) # contains error message...
        return((data.frame(Warning=allVarList[[1]][[1]])))
    nc = 0
    ## get longest list item in idx, length nc = num columns
    for(i in 1:length(allVarList)){
        len = length(allVarList[[i]])
        if(len>nc){
            nc = len
            idx = i
        }
    }
    nr = length(allVarList)             # number of rows
    nm = names(allVarList[[idx]])       # names of longest list item
    rtn = matrix(nrow=nr, ncol=nc)      # matrix filled with NA
    for(i in 1:nr){
        ll = length(allVarList[[i]])
        for(j in 1:ll)
            rtn[i,j] = allVarList[[i]][[j]]
    }
    colnames(rtn) = nm
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

getbigpicturevar = function(){
    if(length(bigpictureVarList)==0)
        return(data.frame(Warning="No variables added yet!"))
    else if(length(bigpictureVarList[[1]] )==1) # contains error message...
        return((data.frame(Warning=bigpictureVarList[[1]][[1]])))
    nc = 0
    for(i in 1:length(bigpictureVarList)){
        len = length(bigpictureVarList[[i]])
        if(len>nc){
            nc = len
            idx = i
        }
    }
    nr = length(bigpictureVarList)
    nm = names(bigpictureVarList[[idx]])       # names of longest list item
    rtn = matrix(nrow=nr, ncol=nc)      # matrix filled with NA
    for(i in 1:nr){
        ll = length(bigpictureVarList[[i]])
        for(j in 1:ll)
            rtn[i,j] = bigpictureVarList[[i]][[j]]
    }
    colnames(rtn) = nm
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

getdefvalues = function(varlist){       # varlist is bigpictureVarList, windowsVarList, etc.
  ## get the default values of all the cells in bigpictureVarList

    if(length(varlist)==0)
        return(data.frame(Warning="No variables added yet!"))
    else if(length(varlist[[1]] )==1) # contains error message...
        return((data.frame(Warning=varlist[[1]][[1]])))
    nc = 4                              # sheet cell desc value
    nr = length(varlist)
    nm = c("sheet","cell","desc","value")
    rtn = matrix(nrow=nr, ncol=nc)      # matrix filled with NA
    colnames(rtn) = nm
    for(i in 1:nr){
        for(j in 1:(nc-1))
            rtn[i,j] = varlist[[i]][[j]]
        rtn[i,4] = round(cellval(varlist[[i]][[1]], varlist[[i]][[2]]),3)
    }
    rtn = data.frame(rtn)
    rtn
}

getwindowsvar = function(){
    if(length(windowsVarList)==0)
        return(data.frame(Warning="No variables added yet!"))
    else if(length(windowsVarList[[1]] )==1) # contains error message...
        return((data.frame(Warning=windowsVarList[[1]][[1]])))
    nc = 0
    for(i in 1:length(windowsVarList)){
        len = length(windowsVarList[[i]])
        if(len>nc){
            nc = len
            idx = i
        }
    }
    nr = length(windowsVarList)
    nm = names(windowsVarList[[idx]])       # names of longest list item
    rtn = matrix(nrow=nr, ncol=nc)      # matrix filled with NA
    for(i in 1:nr){
        ll = length(windowsVarList[[i]])
        for(j in 1:ll)
            rtn[i,j] = windowsVarList[[i]][[j]]
    }
    colnames(rtn) = nm
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

getassembliesvar = function(){
    if(length(assembliesVarList)==0)
        return(data.frame(Warning="No variables added yet!"))
    else if(length(assembliesVarList[[1]] )==1) # contains error message...
        return((data.frame(Warning=windowsVarList[[1]][[1]])))
    nc = 0
    for(i in 1:length(assembliesVarList)){
        len = length(assembliesVarList[[i]])
        if(len>nc){
            nc = len
            idx = i
        }
    }
    nr = length(assembliesVarList)
    nm = names(assembliesVarList[[idx]])       # names of longest list item
    rtn = matrix(nrow=nr, ncol=nc)      # matrix filled with NA
    for(i in 1:nr){
        ll = length(assembliesVarList[[i]])
        for(j in 1:ll)
            rtn[i,j] = assembliesVarList[[i]][[j]]
    }
    colnames(rtn) = nm
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

## take a variable list (as in one element of allVarList, or a return value of getvarlist()
## turn it into a data frame...
getvar = function(varlist){
    nr = 1
    nc = length(varlist)
    nm = names(varlist)       # names 
    rtn = matrix(nrow=nr, ncol=nc)      # matrix filled with NA
    for(i in 1:nr){
        ll = length(varlist)
        for(j in 1:ll)
            rtn[i,j] = varlist[[j]]
    }
    colnames(rtn) = nm
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

greendn = function(y){
    xy = par()$usr
    xmn = xy[1]; xmx = xy[2]; ymn = xy[3]; ymx = xy[4]
    if(y<ymn)                           # should be all red no green
        return(NULL)
    if(y>ymx)                          # color all green
        yy = ymx
    else
        yy = y
    polygon(c(xmn,xmn,xmx,xmx),c(ymn,yy,yy,ymn),col = rgb(0,1,0,0.2),border=NA)
}
redup = function(y){
    xy = par()$usr
    xmn = xy[1]; xmx = xy[2]; ymn = xy[3]; ymx = xy[4]
    if(y>ymx)                           # should be all green no red
        return(NULL)
    if(y<ymn)                          # color all red
        yy = ymn
    else
        yy = y
    polygon(c(xmn,xmn,xmx,xmx),c(yy,ymx,ymx,yy),col = rgb(1,.2,0,0.2),border=NA)
}


shinyServer(function(input, output,session) {
    session$onSessionEnded(function() { # The session ended
        clearserver()
    })

  output$welcome <- renderUI({
      query = parseQueryString(session$clientData$url_search)
      userid <<- query['userid']
      firstname <<- query['firstname']
      db_transact()
      welcomestr = paste('<p> Hi <b> ',firstname,'</b>...  Welcome to PDT Passivhaus optimization!<br/><br/>')
      welcomestr = paste(welcomestr,"<b>Please close this browser window (or tab) when done!</b><br/><br/>")
      welcomestr = paste(welcomestr,"<b>To reload your PHPP workbook, please close this browser window (or tab) to begin again.</b><br/>")      
      HTML(welcomestr)
  })
      initialize()

            
    observe({
        input$infile
        excelinit(input$infile[4])
    })
    observe({
        input$doit                      # creation of new variable
        if(input$doit==0)
          return()                      # button not pushed yet...
        inputfile = isolate(input$infile)
        base=isolate(paste("output/PassivhausOutput-",input$outfile,sep='',gsub(" ","-",date())))
        save(inputfile,allVarList,file=base)
        cmd = paste("cp ",shinyfile,' ',base,".xls",sep='')
        logfoo(paste('Copying...',cmd))
        system(cmd)
    })

    observe({
        input$windows_varfile
        if(is.null(input$windows_varfile))
            return()
        loadVars(input$windows_varfile[4]) # filename from inputted varfile
    })

    observe({
        input$assemblies_varfile
        if(is.null(input$assemblies_varfile))
            return()
        loadVars(input$assemblies_varfile[4]) # filename from inputted varfile
    })
    output$choose_sheet = renderUI({
        selectInput('sheet','Sheet',as.list(sheetnames))
    })
    output$get_varfile= renderUI({
        selectInput('varfile','Choose variable input file:',as.list(varfiles))
    })

    output$scanVarTable = renderTable({  # output for added variable
      input$doit                      # create variable button pushed
      if(input$doit==0)
        return()
      isolate({
                  foo = getvarlist(input) # create the new variable from the spec
                  scanVarList[[length(scanVarList)+1]] <<- foo
                  logfoo(paste('output$scanVarTable:  len scanVarList = ',length(scanVarList)))
                  dat = getvar(foo)       # get the dataframe
                })
        dat
    })
  
    ## output$allDefTable = renderUI({
    ##     isolate({
    ##     dat1 = HTML("<p>Big picture variables:</p>")
    ##     dat2 = getdefvalues(bigpictureVarList)
    ##     dat3 = HTML("<p>Windows variables:</p>")
    ##     dat4 = getdefvalues(windowsVarList)
    ##     dat5 = HTML("<p>Assemblies variables:</p>")
    ##     dat6 = getdefvalues(assembliesVarList)
    ## })
        

        

    output$bigpictureDefTable = renderTable({  # output for added variable
        isolate({
          dat = getdefvalues(bigpictureVarList)
                })
        dat
    })
    output$windowsDefTable = renderTable({  # output for added variable
        isolate({
          dat = getdefvalues(windowsVarList)
                })
        dat
    })
    output$assembliesDefTable = renderTable({  # output for added variable
        isolate({
          dat = getdefvalues(assembliesVarList)
                })
        dat
    })
 
    output$PDTplot = renderPlot({  # plot just scanned 1st big picture variable...
      input$launch
      if(input$launch==0)
        return(NULL)
      ## minchoice()                        # in excel.R
      ## datmin = getEE()                     # --- " ---
      ## maxchoice()                        # in excel.R
      ## datmax = getEE()                     # --- " ---
      ## data.frame(max=datmax,min=datmin)
      isolate({
        logfoo('beginning  plot...............')
        nms = c("Heating demand","Heating load","Cooling load","Primary energy")
        thresh = c(15,10,15,120)
        logfoo(paste('output$PDTplot:  len scanVarList = ',length(scanVarList)))
        if(length(scanVarList)>0){
          vv = scanVarList[[length(scanVarList)]]
          foo = character(0)
          for(i in 1:length(vv))
            foo = c(foo,vv[i])
          logfoo(foo)
        }
        else
          vv = allVarList[[length(allVarList)]]
        ## display progress bar...
        withProgress(message = 'Generating data', detail = "launching excel...", value = 0, {
            sheet = vv$sheet
            cell = vv$cell
            vals = getVar(vv)               # array of values to scan
            eee = numeric(0)
            n <- length(vals)
            for (i in 1:n) {
                incProgress(0.1, detail = paste("value", i,'of',n))
                initVarOutList()            # initialize communication with excel
                addVar(sheet,cell,vals[i])  # prepare sheet, cell, value for excel
                ee = getEE()                # launch excel, get 4 return values
                eee = rbind(eee,ee)
                                        # Increment the progress bar, and update the detail text.
            }
        })

        ## for execution without progress indicator...
        ##        eee = scanVar(vv)                  # return a matrix 4xscan values
            ## sheet = vv$sheet
            ## cell = vv$cell
            ## vals = getVar(vv)               # array of values to scan
            ## eee = numeric(0)
            ## n <- length(vals)
            ## for (i in 1:n) {
            ##     initVarOutList()            # initialize communication with excel
            ##     addVar(sheet,cell,vals[i])  # prepare sheet, cell, value for excel
            ##     ee = getEE()                # launch excel, get 4 return values
            ##     eee = rbind(eee,ee)
            ##                             # Increment the progress bar, and update the detail text.
            ## }
        ## plot on the screen
          par(mfrow = c(2,2))
          for(i in 1:ncol(eee)){      # 4 cols
            ee = eee[,i]
            if(sum(is.na(ee))==length(ee)) # check for all NA that will screw up plot
                ee[] = 0
            xx = unlist(vv[4:(3+length(ee))])
            logfoo(paste('length xx,ee',length(xx),length(ee)))
            logfoo(paste(xx,ee,collapse=' '))
            percvar = 100*(max(ee)-min(ee))/min(ee)
            percdom = 100*(max(xx)-min(xx))/min(xx)
            deriv = round(percvar / percdom , 3)
            xl = paste(vv$desc,"  (",vv$sheet," ! ",vv$cell,")")
            plot(xx,ee,type='b',xlab = xl,ylab = nms[i],family='sans')
            mtext(paste("var = ",round(percvar,1),"%,  deriv = ",deriv,sep=""),family='sans')
            abline(h=thresh[i],lty=2)
            greendn(thresh[i])
            redup(thresh[i])
        }
        ## plot to a file        
        pdf(paste(file='/tmp/Sensitivity',rndtag,'.pdf',sep=''))
        par(mfrow = c(2,2))
        for(i in 1:ncol(eee)){      # 4 cols
            ee = eee[,i]
            if(sum(is.na(ee))==length(ee)) # check for all NA that will screw up plot
                ee[] = 0
            xx = unlist(vv[4:(3+length(ee))])
            percvar = 100*(max(ee)-min(ee))/min(ee)
            percdom = 100*(max(xx)-min(xx))/min(xx)
            deriv = round(percvar / percdom , 3)
            xl = paste(vv$desc,"  (",vv$sheet," ! ",vv$cell,")")
            plot(xx,ee,type='b',xlab=xl,ylab = nms[i])
            mtext(paste("var = ",round(percvar,1),"%,  deriv = ",deriv,sep=""))
            abline(h=thresh[i],lty=2)
        }
        dev.off()
      })
    })

    output$PDTplott = renderPlot({  # plot all variables to /tmp/Sensitivity.pdf
      input$launch
      if(input$launch==0)
        return(NULL)
      ## minchoice()                        # in excel.R
      ## datmin = getEE()                     # --- " ---
      ## maxchoice()                        # in excel.R
      ## datmax = getEE()                     # --- " ---
      ## data.frame(max=datmax,min=datmin)
      pdf(file='/tmp/Sensitivity.pdf')
      logfoo('beginning  plots...............')
      par(mfrow = c(3,4))
      bigpicOpt <<- bigpictureVarList
      for(vv in bigpictureVarList){
#          vv = vv[1:5]                  # for test...
          ee = scanVar(vv)
          ee = as.numeric(ee)
         if(length(vv)>4){             # check for enough scanable entries
              xx = unlist(vv[4:(3+length(ee))])
              logfoo(paste(xx,ee,collapse=' '))
              percvar = 100*(max(ee)-min(ee))/min(ee)
              percdom = 100*(max(xx)-min(xx))/min(xx)
              deriv = round(percvar / percdom , 3)
              plot(xx,ee,type='b',ylab = vv$desc,main='Big Picture',
                   xlab = paste("var = ",round(percvar,1),"%,  deriv = ",deriv,sep=""))
              xmin = xx[ee=min(ee)]
          }
      }
      logfoo("===== done bigpicture.pdf")
      par(mfrow = c(3,4))
      for(vv in windowsVarList){
          ee = scanVar(vv)
          ee = as.numeric(ee)
          if(length(vv)>4){             # check for enough scanable entries
              xx = unlist(vv[4:(3+length(ee))])
              logfoo(paste(xx,ee,collapse=' '))
              percvar = 100*(max(ee)-min(ee))/min(ee)
              percdom = 100*(max(xx)-min(xx))/min(xx)
              deriv = round(percvar / percdom , 3)
              plot(xx,ee,type='b',ylab = vv$desc,main='Windows',
                   xlab = paste("var = ",round(percvar,1),"%,  deriv = ",deriv,sep=""))
          }
      }
      logfoo("===== done windows.pdf")
      par(mfrow = c(3,4))
      for(vv in assembliesVarList){
          ee = scanVar(vv)
          ee = as.numeric(ee)
          if(length(vv)>4){             # check for enough scanable entries
              xx = unlist(vv[4:(3+length(ee))])
              logfoo(paste('length of xx, ee = ',paste(c(length(xx),length(ee)),collapse = ' ')))
              logfoo(paste(xx,ee,collapse=' '))
              percvar = 100*(max(ee)-min(ee))/min(ee)
              percdom = 100*(max(xx)-min(xx))/min(xx)
              deriv = round(percvar / percdom , 3)
              plot(xx,ee,type='b',ylab = vv$desc,main='Assemblies',
                   xlab = paste("var = ",round(percvar,1),"%,  deriv = ",deriv,sep=""))
          }
      }
      logfoo("===== done assemblies.pdf")
      dev.off()
      system('uuencode /tmp/Sensitivity.pdf pdf | mail n@protolife.com -s sensitivities')
      par(mfrow=c(1,1))
      plot(1:10)
 #     dev.off()
    })
    output$bigpictureData <- downloadHandler(
        filename = function() {
            paste('bigpicture-', Sys.Date(), '.RData', sep="")
        },
        content = function(file) {
            save(bigpictureVarList, file=file)
        })
    output$windowsData <- downloadHandler(
        filename = function() {
            paste('windows-', Sys.Date(), '.RData', sep="")
        },
        content = function(file) {
            save(windowsVarList, file=file)
        })
    output$assembliesData <- downloadHandler(
        filename = function() {
            paste('assemblies-', Sys.Date(), '.RData', sep="")
        },
        content = function(file) {
            save(assembliesVarList, file=file)
        })
})
