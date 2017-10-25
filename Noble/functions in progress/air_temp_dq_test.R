air.temp.dq.test<-function(data.dir){
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
