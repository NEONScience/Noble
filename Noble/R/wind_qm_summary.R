# bgn.month="2017-06"
# end.month="2017-08"
# site="GRSM"
.wind.qm.summary=function(site, bgn.month, end.month, save.dir){
    if(missing(save.dir)){save.dir=tempdir()}
    data<-data.pull(site = site, dpID = "DP1.00001.001", bgn.month = bgn.month, end.month = end.month, time.agr = time.agr, package="expanded", save.dir = save.dir)

    dir.QM.only=data[,(grepl(colnames(data), pattern = "windDir") & grepl(x=colnames(data), pattern = "QM"))]

    qm.sums=data.frame(startDateTime=NA,rbind(colSums(dir.QM.only>20, na.r=T)))
    qm.count.avgs=Noble::un.ml.ize(qm.sums)
    qm.count.percent=round(qm.count.avgs/length(data[,1])*100, digits = 2)
    qm.count.percent=qm.count.percent[,-1]
    qm.count.percent=data.frame(Site=site, qm.count.percent)
    return(qm.count.percent)
}
