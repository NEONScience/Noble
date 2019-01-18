precip.plot=function(site, bgn.month, end.month, cumulative=T){

    type=Noble::tis_site_config$Core.Relocatable[Noble::tis_site_config$SiteID==site]

    if(type=="Core"){
        precip=Noble::data.pull(site = site, dpID = "DP1.00006.001", bgn.month = bgn.month, end.month = end.month, time.agr = 5, package = "basic", save.dir = tempdir(), complete.times = F)
    }
    if(type=="Relocatable"){
        precip=Noble::data.pull(site = site, dpID = "DP1.00006.001", bgn.month = bgn.month, end.month = end.month, time.agr = 1, package = "basic", save.dir = tempdir(), complete.times = F)
    }

    precip$cumulative=cumsum(precip$TFPrecipBulk.001.000)
    precip$startDateTime=as.POSIXct(precip$startDateTime, tz="UTC")
    colnames(precip)[3]="Precip"

    if(cumulative){
        plot=ggplot2::ggplot(data=precip, ggplot2::aes(x=startDateTime, y = cumulative))+
            ggplot2::geom_path(colour="#074f82")+
            ggplot2::labs(title=paste0(site, " Precipitaion (cumulative)"), x="Date", y="Precipitaion (mm)")+
            ggplot2::theme_light()
    }
    if(!cumulative){
        plot=ggplot2::ggplot(data=precip, ggplot2::aes(x=startDateTime, y = Precip))+
            ggplot2::geom_path(colour="#074f82")+
            ggplot2::labs(title=paste0(site, " Precipitaion (Instantaneous)"), x="Date", y="Precipitaion (mm)")+
            ggplot2::theme_light()
    }
    return(plot)
}
