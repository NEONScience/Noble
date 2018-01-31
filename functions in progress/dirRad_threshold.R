############################################################################################
# title  Calculates a thresholds for direct radiation

# author Robert Lee \email{rlee@battelleecology.org}\cr

# changelog and author contributions / copyrights
#   Robert Lee (2017-07-10)
#     original creation
#
##############################################################################################

.dirRad.threshold = function(site, bgn.month, end.month, excuse){
    require(RAtmosphere)

    lat=Noble::tis_site_config$Latitude[which(Noble::tis_site_config$SiteID==site)]
    lon=Noble::tis_site_config$Longitude[which(Noble::tis_site_config$SiteID==site)]
    bgn.date = as.Date(paste0(bgn.month, "-01"), tz="UTC")
    end.date = Noble::end.day.time(end.month = end.month, time.agr = 1440)
    end.date
    dates = Noble::help.time.seq(from = bgn.date, to = end.date, time.agr = 1440)
    dates=as.POSIXlt(dates)

    julian.dates=dates$yday

    sun.times=lapply(julian.dates, function(x) RAtmosphere::suncalc(d = x, Lat = lat, Long = lon, UTC = TRUE))
    sun.times=data.frame(cbind(julian.dates, do.call(rbind, sun.times)))
    sink=lapply(c(1, 2, 3), function(x) class(sun.times[,x])<-"numeric")
    rm(sink)
    mean.sun.up=mean(unlist(sun.times$sunrise))
    mean.sun.down=mean(unlist(sun.times$sunset))

    threshold=round(((mean.sun.down-mean.sun.up)/24)*100-excuse, digits=2)
    return(threshold)
}

