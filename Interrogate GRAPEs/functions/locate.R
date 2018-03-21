locate=function(site, bgn, end){
    if(missing(site)){stop("No site entered. Please input a 4-letter site code. ")}
    
    bgn=as.Date(bgn)
    end=as.Date(end)
    
    site=toupper(site)
    #Routing of external drives based on OS:
    if(grepl("darwin", version$os))
    {
        base.dir<-"/Volumes/neon/Common/ENG/Sites/Sensors/" #Mac
    }else{
        base.dir<-"N:/Common/ENG/Sites/Sensors" #Windows
    }
    
    log.dir=paste0(base.dir, "data/", site, "/")
    map.dir=paste0(base.dir, "maps/", site, "/")
    
    avail.logs=list.files(log.dir, pattern = ".csv")
    avail.maps=list.files(map.dir, pattern = ".csv")
    
    log.dates=lapply(stringr::str_split(avail.logs, "_"), function(x) as.Date(x[2], format="%Y%m%d"))
    map.dates=lapply(stringr::str_split(avail.maps, "_"), function(x) as.Date(x[2], format="%Y%m%d"))
    req.logs=paste0(log.dir, avail.logs[log.dates<=end&log.dates>=bgn])
    req.maps=paste0(map.dir, avail.maps[map.dates<=end&map.dates>=bgn])
    
    files=mapply(c, req.logs, req.maps, SIMPLIFY = FALSE)
    
    names(files)=NULL
    return(files)
}