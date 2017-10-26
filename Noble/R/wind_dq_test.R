# #Test Block
# site="CPER"
#
# bgn.month="2017-07"
# end.month="2017-08"
#
# test = "TIS 2D Wind Speed and Direction Data Quality "
# testSubDir = "Tis2DWindSpeedDataQuality"
# if(grepl("darwin", version$os))
# {
#     mountPoint<-"/Volumes/neon/" #Mac
# }else{
#     mountPoint<-"N:/" #Windows
# }
# dirCommBase = paste0(mountPoint, "Science/Science Commissioning Archive/SiteAndPayload/")
# testFullDir=paste0(dirCommBase, testSubDir, "/")
# save.dir=testFullDir
#
# #API not working
# data=read.csv(paste0(site.dir, list.files(site.dir)[1]))

# Wind DQ Test

wind.dq.test=function(site, save.dir, bgn.month, end.month){
    domn=Noble::is_site_config$Domain[Noble::is_site_config$SiteID==site]
    site.dir=paste0(save.dir, "/", domn, "-", site, "/")

    site.MLs=1:Noble::tis_site_config$Num.of.MLs[Noble::tis_site_config$SiteID==site]
    wind.MLs=site.MLs[-length(site.MLs)]

    rslt.dir=paste0(save.dir, "/", "Common/")
    if(!dir.exists(rslt.dir)){
        dir.create(rslt.dir)
    }
    data=Noble::data.pull(site = site, dpID = "DP1.00001.001", bgn.month = bgn.month, end.month = end.month, time.agr = 30, package = "basic", save.dir = site.dir)

    speed=data.frame(data[,grepl(x=colnames(data), "windSpeedMean*")])
    dir=data.frame(startDateTime=data$startDateTime, data[,grepl(x=colnames(data), "windDirMean*")])

    #ML internal comparison
    ML.pairwise=lapply(wind.MLs[-1], function(x) c(x-1, x))
    comp=lapply(ML.pairwise, function(x) cor(speed[,x[1]], speed[,x[2]], use = "complete.obs"))

    stats::cor(x=)
}
