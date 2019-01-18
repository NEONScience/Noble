
## Private function, because it requires being on the NEON intranet

.grape.check<- function(gap.df, site){
##### 1  read gap.df in
    # format should be:
    #  "index | gap.start            | gap.end"
    #  --------------------------------------------------
    #       1 | 2016-01-01T00:00:00 | 2017-01-01T00:00:00     etc...
    # Example dataframe:
    TimeBgn1<-as.character.Date("2017-05-01 00:12:30", format="%Y-%m-%d %H:%M:%S", tz="UTC")
    TimeBgn2<-as.character.Date("2017-05-10 00:13:30", format="%Y-%m-%d %H:%M:%S", tz="UTC")
    TimeEnd1<-as.character.Date("2017-05-03 00:10:00", format="%Y-%m-%d %H:%M:%S", tz="UTC")
    TimeEnd2<-as.character.Date("2017-05-11 00:11:00", format="%Y-%m-%d %H:%M:%S", tz="UTC")
    gap.df<-data.frame("index"=c(1, 2), "gap.start"=c(TimeBgn1, TimeBgn2), "gap.end" = c(TimeEnd1, TimeEnd2))
    # Example site to use:
    site <- "CPER"
    #


##### 2  Determine what days are in the gapped periods
    gap.seq<-c()
    for(x in 1:length(gap.df$index)){
        gap.seq<-append(gap.seq, list(seq.Date(from = as.Date(gap.df$gap.start[x]), to = as.Date(gap.df$gap.end[x]), by = "days")))
    }

##### 3  convert dates to the format found grape log files.
    log.dates<-unlist(lapply(gap.seq, function(x) as.character(x-1)))
    log.dates<-gsub(x=log.dates, pattern = "-", replacement = "")

    #    *Note dates there are a day ahead of the grape date (eg 2017-01-01 corresponds to 2016-12-31)

##### 4  Find all grapes where uptime is less than 95% in the gap period
    grape.dir <- "/Users/rlee/Dropbox/NEON/SOM/data"
    grape.site.dir <- list.dirs(grape.dir)[which(grepl(list.dirs(grape.dir), pattern = site))]
    log.files <- list.files(grape.site.dir)

    grape.log.dates<-substr(log.files, 6, 13)

    need.indx <- grape.log.dates %in% log.dates

    bad.grapes<-c()
    for(i in 1:length(log.files[need.indx])){
        temp.log<-read.csv(paste0(grape.site.dir, "/", log.files[need.indx][i]), header = F)
        temp.grape.ids <- temp.log[which(trimws(temp.log[,2]) %in% temp.log[,5]),]
        bad.grapes <- append(bad.grapes, trimws(temp.log[which(temp.log$V3<0.8),2]))
    }
    ## NEED TO ALSO ADD A CHECK FOR GRAPES THAT DROPPED OFF-CHECK THE DAY BEFORE THE GAP AND COMPARE TO SEE WHICH MAC IS NO LONGER THERE


#### 5  Output list of suspect grapes
}
