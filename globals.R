


vnames = paste("v",1:10,sep='')

allVarList = list()
bigpictureVarList = list()
windowsVarList = list()
assembliesVarList = list()
scanVarList = list()

shinyfile = ''
sheetnames = NULL
varfiles = NULL
rndtag = 0
logfile = ''

## DB stuff
userid = 0
firstname = ''
servername = ''
transid = 0
mydb = NULL


logfoo = function(str=''){
    cat(file=logfile,paste(str,'\n'),append=TRUE)
}

