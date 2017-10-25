require(zoo)

dpID="DP1.00001.001"

dp.avail = NEON.avail(dpID = dpID)
dp.sites = colnames(dp.avail[,2:length(colnames(dp.avail))])
dp.sites="CPER"
for(i in 1:length(dp.sites)){
    temp.dates = zoo::as.Date(dp.avail$Month[which(dp.avail[,i+1]=="x")])
    site=colnames(dp.avail)[i+1]
    run.dates = substr(temp.dates, start = 0, stop = 7)
    for(d in 1:length(run.dates)){
        month.data<-try(Noble::data.pull(site = dp.sites[i], dp.name = dpID, bgn.month = run.dates[d], end.month = run.dates[d], time.agr = 30, package = "basic", save.dir = tempdir()))
        #Need to have dp Index, QF index
        #total.data<-month.data[which(!is.na(month.data[,3])),3]
    }
}
