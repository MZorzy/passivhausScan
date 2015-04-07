

library('xlsx')
source('globals.R')
shinyfile <<- ''                          # file containing  current spreadsheet.
varfile <<- ''
wb = NULL                               # current workbook
sheets =NULL                            # sheets of current workbook
                                        # all set in excelinit()
# sheetnames in globals.R

excelinit = function(infile)
{
    if(length(infile)==0)
        return(NULL)
    if(is.na(infile))
        return(NULL)
    if(infile == "" || is.null(infile))
        return(NULL)
    rnd = round(10000*abs(runif(1)),0)
    shinyfile <<- paste(system('pwd',intern=T),'/output/shiny',rnd,'.xls',sep='')
    system(paste('cp ',infile,shinyfile))
    system(paste('chmod 777 ',shinyfile))
#    unprotect in applescript...
#    cmd = paste('UnprotectXls',shinyfile)
#    logfoo(paste('------------ unprotecting:  ',cmd))
#    system(cmd,intern=TRUE)
    wb <<- loadWorkbook(shinyfile)
    sheets <<- getSheets(wb)
    sheetnames <<- names(sheets)
}

checksheet = function(str)
{
    if( str %in% names(sheets))
        return (TRUE)
    else
        return (FALSE)
}

letters = "abcdefghijklmnopqrstuvwxyz"
letters = strsplit(letters,NULL)[[1]]   # vector of letters
dletters = character()
for(i in 1:6){
    dletters = c(dletters,paste(letters[i],letters,sep=''))
    }
letters = c(letters,dletters)

checknumeric = function(cc)
    {
        suppressWarnings(!is.na(as.numeric(cc)))
    }

convertcelladr = function(str)          # e.g. str = 'a3',  'C5', etc.
{
    chars = strsplit(str,NULL)[[1]]
    rowchar = character(0)
    colchar = character(0)
    start = 1
    gotnum = 0
    for(cc in chars){
        cc = tolower(cc)          # e.g. to take A3 -> a3
        if(start==1 && checknumeric(cc) == TRUE){
            return(NA)                  # start out with number => bad
        }
        else
            start = 0
        if(checknumeric(cc) == TRUE){
            rowchar = paste(rowchar,cc,sep='')
            gotnum = 1
        }
        if(checknumeric(cc) == FALSE){
            colchar = paste(colchar,cc,sep='')
            if(gotnum==1)
                return(NA)
        }
    }
    if(is.na(match(colchar,letters))){
        return(NA)                        # colchar must be acceptable address string 
    }
    else
        colnum = match(colchar,letters)
    rownum = as.numeric(rowchar)
    return(c(rownum,colnum))
}    

checkcell = function(sheetname,cell,mn,mx)
{
    if(is.null(wb))
        return('noload')
    if(checksheet(sheetname)==FALSE)
        return('badsheet')
    sheet = sheets[[sheetname]]
    rows = getRows(sheet)
    rnums = as.numeric(names(rows))
    celladr = convertcelladr(cell)
    if(is.na(match(celladr[1],rnums)))    # row not found
        return('badrow')
    else
        rnum = celladr[1]
    cnum = celladr[2]

    rr = rows[[match(as.character(rnum),names(rows))]]
    val = getCellValue(getCells(list(rr),cnum)[[1]])
    ##  don't flag missingval and non-numeric, because we may want to overwrite derived variables...
    ## if(is.na(val))
    ##     return('missingval')
    ## if(!is.numeric(val))
    ##     return(FALSE)
    ## if(val<mn || val > mx)
    ##     return('outofrange')
    return(TRUE)
}


cellval = function(sheetname,cell)
{
    if(is.null(wb))
        return('noload')
    if(checksheet(sheetname)==FALSE)
        return('badsheet')
    sheet = sheets[[sheetname]]
    rows = getRows(sheet)
    rnums = as.numeric(names(rows))
    celladr = convertcelladr(cell)
    if(is.na(match(celladr[1],rnums)))    # row not found
        return('badrow')
    else
        rnum = celladr[1]
    cnum = celladr[2]

    rr = rows[[match(as.character(rnum),names(rows))]]
    val = getCellValue(getCells(list(rr),cnum)[[1]])
    if(is.na(val))
        return('missingval')
    if(!is.numeric(val))
        return(NA)
    return(val)
}

################################################################
## below are functions for writing variable data to a file, and getting
## spreadsheet values (e.g. getEE()) back from a spreadsheet that has those
## variables substituted.
##
## VarOutList is built up one variable at a time with addVar().
## Each variable specified by sheet, cell, value.
## getEE() writes out VarOutList, then calls the Applescript OpenXls to put the data in and retrieve the EE value.

varOutList = list()

initVarOutList = function(){
    varOutList <<- list()
}

saveVarOutList = function(){
    varfile <<- paste(system('pwd',intern=T),'/output/tmpshiny.var',sep='')
    system(paste('rm -rf',varfile))
    for(vv in varOutList){
        cat(vv,'\n',file=varfile,append=T)
      }
}

addVar = function(sheet,cell,value){
    varOutList[[length(varOutList)+1]] <<- paste(sheet,cell,value,sep=',')
}

getEE = function(){
    saveWorkbook(wb,shinyfile)
    saveVarOutList()
    ## call OpenXls to open excel on shinyfile with variable substitutions
    ## specified in varfile.
    ## note full pathe needed for OpenXls script.
    cmd = paste('/srv/shiny-server/passivhausscan/OpenXls',shinyfile,varfile)
#    warning(paste('--- getEE: cmd = ',cmd))
#    logfoo(paste('--- getEE: cmd = ',cmd))
    foo = system(cmd,intern=TRUE)
    logfoo(paste('getEE:   --- system cmd = ',cmd))
    ## foo is a string with four numbers
    ##
    ## Specific Space Heating Demand or Cooling Demand
    ## or Specific Heat/Cooling Load
    ## Primary energy
    ## Airtightness
#    warning('--- done with excel call')
    logfoo(paste('getEE:   --- OpenXls output = ',foo))
    foo = as.numeric(foo)
#    foo = as.numeric(unlist(strsplit(foo,' ')))
    logfoo(paste('getEE:   --- rtn = ',foo))
    logfoo(paste('getEE:   --- len of rtn = ',length(foo)))
    foo
}
  
minchoice = function(){
    initVarOutList()
    for(vv in allVarList){
        sheet = vv$sheet
        cell = vv$cell
        vals = unlist(vv[4:length(vv)])
        val = vals[1]                       # min val
        addVar(sheet,cell,val)
    }
}
maxchoice = function(){
    initVarOutList()
    for(vv in allVarList){
        sheet = vv$sheet
        cell = vv$cell
        vals = unlist(vv[4:length(vv)])
        for( i in length(vals):1){
            val = vals[i]
            if( ! is.na(val))
                break
        }
        addVar(sheet,cell,val)
    }
}

getVar = function(vv){          # vv is e.g. an element of allVarList
    sheet = vv$sheet
    cell = vv$cell
    vals = unlist(vv[4:length(vv)])
    for( i in length(vals):1){
        val = vals[i]
        if( ! is.na(val))
            break
    }
    vals = vals[!is.na(vals)]
    vals = as.numeric(vals)
    vals
  }

scanVar = function(vv){          # vv is e.g. an element of allVarList
    sheet = vv$sheet
    cell = vv$cell
    vals = unlist(vv[4:length(vv)])
    for( i in length(vals):1){
        val = vals[i]
        if( ! is.na(val))
            break
    }
    vals = vals[!is.na(vals)]
    res = numeric(0)
    for(v in vals){
        initVarOutList()
        addVar(sheet,cell,v)
        res = rbind(res,getEE())
    }
    logfoo(paste('scanvar:   --- res matrix',ncol(res),nrow(res),'col,row'))
    ## res is a matrix that contains 4 columns one for each return value of getEE()
    res
}
    









###########################################################
##  below is pre-Applescript code
##  deprecated...

# make a choice of variables from allVarList.
# install them in the spreadsheet
# save the spreadsheet in shinyfile.xls

ranchoice = function(){
  for(vv in allVarList){
    celladr = convertcelladr(vv$cell)
    logfoo(paste('---------- trying to get sheet ',vv$sheet))
    sheet = sheets[[vv$sheet]]
    vals = unlist(vv[4:length(vv)])
    idx = sample(1:length(vals),1)
    val = vals[idx]                     # random sample of range

    rows = getRows(sheet)
    rnums = as.numeric(names(rows))

    if(is.na(match(celladr[1],rnums)))    # row not found
        return('badrow')
    else
        rnum = celladr[1]
    cnum = celladr[2]
    rr = rows[[match(as.character(rnum),names(rows))]]
    cell = getCells(list(rr),cnum)[[1]]
    logfoo(paste('========= trying to set cell',vv$cell,rnum,cnum,'to value',val))
    logfoo(paste('========= rowcheck:',length(rr)))
    logfoo(paste('========= cellcheck:',length(cell)))
    setCellValue(cell,val)
  }
}

maxchoicex= function(){
  for(vv in allVarList){
    celladr = convertcelladr(vv$cell)
    logfoo(paste('---------- trying to get sheet ',vv$sheet))
    sheet = sheets[[vv$sheet]]
    vals = unlist(vv[4:length(vv)])
    idx = length(vals)
    val = vals[idx]                     # max val

    rows = getRows(sheet)
    rnums = as.numeric(names(rows))

    if(is.na(match(celladr[1],rnums)))    # row not found
        return('badrow')
    else
        rnum = celladr[1]
    cnum = celladr[2]
    rr = rows[[match(as.character(rnum),names(rows))]]
    cell = getCells(list(rr),cnum)[[1]]
    logfoo(paste('========= trying to set cell',vv$cell,rnum,cnum,'to value',val))
    logfoo(paste('========= rowcheck:',length(rr)))
    logfoo(paste('========= cellcheck:',length(cell)))
    setCellValue(cell,val)
  }
}


setval = function(sheetname,cellname,val){
    celladr = convertcelladr(cellname)
    sheet = sheets[[sheetname]]
    rows = getRows(sheet)
    rnums = as.numeric(names(rows))

    if(is.na(match(celladr[1],rnums)))    # row not found
        return('badrow')
    else
        rnum = celladr[1]
    cnum = celladr[2]
    rr = rows[[match(as.character(rnum),names(rows))]]
    cell = getCells(list(rr),cnum)[[1]]
    ## logfoo(paste('========= trying to set cell',vv$cell,rnum,cnum,'to value',val))
    ## logfoo(paste('========= rowcheck:',length(rr)))
    ## logfoo(paste('========= cellcheck:',length(cell)))
    setCellValue(cell,val)
}    


minchoicex = function(){
  for(vv in allVarList){
    celladr = convertcelladr(vv$cell)
    logfoo(paste('---------- trying to get sheet ',vv$sheet))
    sheet = sheets[[vv$sheet]]
    vals = unlist(vv[4:length(vv)])
    val = vals[1]                       # min val

    rows = getRows(sheet)
    rnums = as.numeric(names(rows))

    if(is.na(match(celladr[1],rnums)))    # row not found
        return('badrow')
    else
        rnum = celladr[1]
    cnum = celladr[2]
    rr = rows[[match(as.character(rnum),names(rows))]]
    cell = getCells(list(rr),cnum)[[1]]
    logfoo(paste('========= trying to set cell',vv$cell,rnum,cnum,'to value',val))
    logfoo(paste('========= rowcheck:',length(rr)))
    logfoo(paste('========= cellcheck:',length(cell)))
    setCellValue(cell,val)
  }
}

