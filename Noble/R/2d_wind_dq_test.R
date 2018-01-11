# bgn.month="2017-07"
# end.month="2017-08"
# site="CPER"
# save.dir=tempdir()

# changelog and author contributions / copyrights
#   Robert Lee (2017-12-14)
#     original creation
#
##############################################################################################

.wind.dq.test<-function(data.dir){

    test.data=Noble::data.pull(site = site, dpID = "DP1.00001.001", bgn.month = bgn.month, end.month = end.month, time.agr = 30, package = "basic", save.dir = save.dir)
    test.data$startDateTime=as.POSIXct(test.data$startDateTime, tz="UTC")

    ## PART 1: Internal Consistancy - Speed
    internal.speed.data=test.data[,grepl(pattern = "windSpeedMean", x = colnames(test.data))]
    #internal.data=internal.data[,!(grepl(pattern = "000.010", x=colnames(internal.data)))]

    speed.spearman=lapply(c( 1:(length(internal.speed.data)-1)), function(l) cor.test(x = internal.speed.data[,l], y = internal.speed.data[,l+1], method = "spearman", conf.level = 0.01))
    mls=unlist(lapply(c( 1:(length(internal.speed.data)-1)), function(x) paste0("ML ", x, "-", x+1)))
    speed.spearman=do.call(rbind, speed.spearman)
    speed.spearman=cbind(Pair=mls, speed.spearman)

    ## PART 3: External Consistancy

    PQ.results<- read.csv(paste0(data.dir,"/results.csv"))
    listed.sites<-unique(PQ.results$site)
    testable.sites<-c()

    for(i in 1:listed.sites)
    {
        site.results<-(PQ.results[PQ.results$site==listed.sites[i],])
        site.results<-site.results[,-2]
    }

    testable.sites<-PQ.results[as.numeric(PQ.results$data_quantity)>80,]

    s}
