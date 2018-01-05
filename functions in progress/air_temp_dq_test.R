bgn.month="2017-07"
end.month="2017-08"
site="CPER"
save.dir=tempdir()

air.temp.dq.test<-function(data.dir){

    ## PART 1: Variance Stability
    saat.test.data=Noble::data.pull(site = site, dpID = "DP1.00002.001", bgn.month = bgn.month, end.month = end.month, time.agr = 30, package = "basic", save.dir = save.dir)
    taat.test.data=Noble::data.pull(site = site, dpID = "DP1.00003.001", bgn.month = bgn.month, end.month = end.month, time.agr = 30, package = "basic", save.dir = save.dir)

    test.data=cbind(saat.test.data, taat.test.data[,(3:length(colnames(taat.test.data)))])
    test.data$startDateTime=as.POSIXct(test.data$startDateTime, tz="UTC")
    attributes(test.data$startDateTime)$tzone=Noble::tis_site_config$Time.Zone[(Noble::tis_site_config$SiteID==site)]

    # Make a sequence of dates and times for the requested period
    bgn_temp <- as.Date(paste0(bgn.month, "-01"), tz="UTC")
    end_temp <- as.Date(paste0(end.month, "-01"), tz="UTC")
    bgn_temp <- as.POSIXct(paste0(bgn.month, "-01"), tz="UTC")
    end_temp<- as.POSIXlt(paste0(end_temp, "-01"), tz="UTC")
    end_temp$mon<-end_temp$mon+1
    #end_temp<-end_temp-lubridate::minutes(30)-lubridate::seconds(1)


    group.one=Noble::date.extract(data = test.data, bgn.date = as.Date(paste0(bgn.month, "-01")), end.date =as.Date(paste0(bgn.month, "-01"))+15)
    group.one=data.frame(startDateTime=group.one$startDateTime, group.one[,grepl(pattern = "tempSingleMean", x = colnames(group.one))|grepl(pattern = "tempTripleMean", x = colnames(group.one))])
    group.one=group.one[lubridate::hour(group.one$startDateTime) %in% c(0:4),]
    group.two=Noble::date.extract(data = test.data, bgn.date = end_temp-lubridate::days(15), end.date = end_temp-lubridate::minutes(30)-lubridate::seconds(1))
    group.two=data.frame(startDateTime=group.two$startDateTime, group.two[,grepl(pattern = "tempSingleMean", x = colnames(group.two))|grepl(pattern = "tempTripleMean", x = colnames(group.two))])
    group.two=group.two[lubridate::hour(group.two$startDateTime) %in% c(0:4),]

    f.test=var.test(x=unlist(as.list(group.one[2:length(group.one)])), y=unlist(as.list(group.two[2:length(group.two)])), conf.level = 0.99)

    ## PART 2: Internal Consistancy
    internal.data=test.data[,grepl(pattern = "tempSingleMean", x = colnames(test.data))|grepl(pattern = "tempTripleMean", x = colnames(test.data))]
    internal.data=internal.data[,!(grepl(pattern = "000.010", x=colnames(internal.data)))]

    spearman=lapply(c( 1:(length(internal.data)-1)), function(l) cor.test(x = internal.data[,l], y = internal.data[,l+1], method = "spearman"))
    mls=unlist(lapply(c( 1:(length(internal.data)-1)), function(x) paste0("ML ", x, "-", x+1)))
    spearman=do.call(rbind, spearman)
    spearman=cbind(Pair=mls, spearman)

    ## PART 3: External Consistancy

    out=c("Variance"=f.test, "Internal"=spearman)
    return(out)
}
