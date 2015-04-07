
library("RMySQL")
source('globals.R')

db_init = function(){
    servername <<- system('whoami',intern=T)
    m<-dbDriver("MySQL");
#    mydb<<-dbConnect(m,user='protolif_N',password='cplexPrtLf.1',host='protolife.com',dbname='protolif_test1');
    mydb<<-dbConnect(m,user='protolif_N',password='cplexPrtLf.1',host='protolife.com',dbname='protolif_passivhaus');


    ## check to make sure servername is registered in the db:
    querystr = paste("select `Availability` from ShinyServers WHERE `ShinyServers`.`ServerName` = '",servername,"'",sep='')
    res = fetch(dbSendQuery(mydb,querystr))
    if(res==1)
        return(TRUE)
    else{
        warning(paste('res = ',res))
        return(FALSE)
    }
        
}

db_transact = function(){
    ## register transaction
    ## need global variables userid and firstname filled from
    ## URL query string
    timest = as.character(Sys.time())
    transquerystr = paste("INSERT INTO  `protolif_passivhaus`.`TransactionsAlpa` (`transid` ,`user_id` ,`first_name` ,`ServerName` ,`trans_begin`) VALUES (NULL,  '",userid,"',  '",firstname,"',  '",servername,"',  '",timest,"');",sep='')
    res = dbSendQuery(mydb,transquerystr)
    querystr = paste("select `transid` from `protolif_passivhaus`.`TransactionsAlpa` where `protolif_passivhaus`.`TransactionsAlpa`.`trans_begin` = '",timest,"' LIMIT 1 ;",sep='')
    transid <<- fetch(dbSendQuery(mydb,querystr))
}
    
    

db_reset = function(sname){
    ## update ShinyServer table
    querystr = paste("UPDATE  `protolif_passivhaus`.`ShinyServers` SET  `Availability` =  '0' WHERE  `ShinyServers`.`ServerName` =  '",sname,"' LIMIT 1 ;",sep='')
    res<-dbSendQuery(mydb, querystr)
    ## update TransactionsAlpa table
    timest = as.character(Sys.time())
    querystr = paste("UPDATE  `protolif_passivhaus`.`TransactionsAlpa` SET  `trans_end` =  '",timest,"' WHERE  `protolif_passivhaus`.`TransactionsAlpa`.`transid` =  '",transid,"' LIMIT 1 ;",sep='')
    res<-dbSendQuery(mydb, querystr)
}    

clearserver = function(){
    db_reset(servername)                # servername set in db_init
}
    

## dbListTables(mydb)
## res<-dbSendQuery(mydb, "select * from ShinyServers")
## fetch(res)


## firstname = 'Harry'
## userid = 6
## timest = as.character(Sys.time())
## querystr = paste("INSERT INTO  `protolif_passivhaus`.`TransactionsAlpa` (`transid` ,`user_id` ,`first_name` ,`serverurl` ,`trans_date`) VALUES (NULL,  '",userid,"',  '",firstname,"',  'whocares',  '",timest,"');")
## dbSendQuery(mydb,querystr)
## fetch(dbSendQuery(mydb, "select * from TransactionsAlpa"))
