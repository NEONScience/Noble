### Return a list of GRAPEs found in log, with a breakdown of what sensors are connected


marry=function(log.file, map.file){
    
    print(log.file)#
    print(map.file)
    
    log.meta=unlist(stringr::str_split(string=log.file, pattern = "/"))
    log.meta=unlist(stringr::str_split(string = log.meta[grepl(x = log.meta, pattern = "_")], pattern = "_"))
    site=log.meta[1]
    date=as.character(as.Date(log.meta[2], format="%Y%m%d"))
    map.meta=stringr::str_split(string=log.file, pattern = "_|/")
    
    
    site=log.meta[duplicated(log.meta)]
    #date=as.character.Date()
    #Define the class 'grape'
    grapes=setClass("grape", contains = c("list"), slots = c(grape="list", date="character", site="character"))
    
    #define the log colnames
    col.names=c("Row", "ID", "Connections", "Uptime", "MAC")
    
    
    log=read.csv(log.file, header = F, col.names = col.names, stringsAsFactors = F)
    map=read.csv(file = map.file, stringsAsFactors = F)
    
    #clean up the log
    log=data.frame(sapply(log, trimws))
    log[,(colnames(log) %in% c("Connections", "Uptime"))] = as.numeric(unlist(log[,(colnames(log) %in% c("Connections", "Uptime"))]))
    g.list=unique(log$MAC[log$MAC==log$ID])
    
    g.indx=which(log$MAC==log$ID)
    sensor=log$ID[-g.indx]
    
    print(colnames(map))
    
    ## Clean up the current site map
    map$MAC=gsub(x = map$MAC, pattern = ":", replacement = "")
    map$ASSET.UID=as.character(map$ASSET.UID)
    map$ASSET.TAG=as.character(map$ASSET.TAG)
    # 
    # map$MAC %in% g.list
    # map$ASSET.UID %in% sensor
    
    
    grape.con=lapply(g.list, function(x) map[map$MAC==x,])
    
    grape.con=list(grape=grape.con, date=date, site=site)
    names(grape.con$grape)=g.list
    
    class(grape.con)="grapes"
    
    return(grape.con)
    
}