#test block
# site="ABBY"
# bgn.month="2018-03"
# end.month="2018-03"
#
#

precip.times=function(site, bgn.month, end.month){
    type=Noble::tis_site_config$Core.Relocatable[Noble::tis_site_config$SiteID==site]

    ## Below function from answer provided by Joris Meys, on stackoverflow.com.
    ## Retrieved Dec 20, 2017 from https://stackoverflow.com/questions/7077710/sequence-length-encoding-using-r?rq=1
    seq.length <- function(x){
        if(!is.numeric(x)) x <- as.numeric(x)
        n <- length(x)
        y <- x[-1L] != x[-n] + 1L
        i <- c(which(y|is.na(y)),n)

        data.frame(
            start_index = as.vector(x[head(c(0L,i)+1L,-1L)]),
            stop_index = as.vector(diff(c(0L,i)))
        )
    }

    if(type=="Core"){
        precip=Noble::data.pull(site = site, dpID = "DP1.00006.001", bgn.month = bgn.month, end.month = end.month, time.agr = 5, package = "basic", save.dir = tempdir(), complete.times = F)
    }
    if(type=="Relocatable"){
        precip=Noble::data.pull(site = site, dpID = "DP1.00006.001", bgn.month = bgn.month, end.month = end.month, time.agr = 1, package = "basic", save.dir = tempdir(), complete.times = F)
    }

    rain.index=row.names(precip[!precip[,3]==0&!is.na(precip[,3]),])
    rain.times=seq.length(rain.index)
    rain.times$stop_index=(rain.times$start_index+rain.times$stop_index)
    rain.times$start_time=precip$startDateTime[rain.times$start_index]
    rain.times$stop_time=precip$startDateTime[rain.times$stop_index]

    for(i in 1:nrow(precip)){
        precip$totals[i]=sum(precip[1:i,3], na.rm = T)
    }


    ggplot2::ggplot(rain.times,
                    ggplot2::aes(x="Precipitaion",
                                 ymin=as.POSIXct(start_time),
                                 ymax=as.POSIXct(stop_time)
                    )
    )+
        ggplot2::geom_linerange(size=10, colour="blue")+
        ggplot2::coord_flip()+
        ggplot2::scale_x_discrete(name="", breaks=NULL)+
        ggplot2::theme_minimal()+
        ggplot2::guides(fill=FALSE)+
        ggplot2::ggtitle(label = site, subtitle = "Precipitation")#+
    # ggplot2::theme(legend.position="none")

}
