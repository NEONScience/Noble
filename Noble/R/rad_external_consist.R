.rad.external.consist=function(){

    if(site=="BART"){
        time.zone=Noble::tis_site_config$Time.Zone[Noble::tis_site_config$SiteID==site]

        offset=difftime(time1 = as.POSIXct("2018-01-01", tz=time.zone), time2=as.POSIXct("2018-01-01", tz="UTC"))

        temp=RNRCS::grabNRCS.data(network = "SCAN",
                                  site_id = 2069,
                                  timescale = "hourly",
                                  DayBgn =paste0(bgn.month, "-01"),
                                  DayEnd =stringr::str_sub(string =  as.character(Noble::end.day.time(end.month = end.month, time.agr = 1)), start = 1, end = 10)
        )
        ext.data=temp
        ext.data$Date=as.POSIXct(ext.data$Date, tz=time.zone)

        int.data=try(Noble::data.pull(site = site,
                                      dpID = "DP1.00014.001",
                                      bgn.month = bgn.month,
                                      end.month = end.month,
                                      time.agr = 30,
                                      package = "basic",
                                      save.dir = Noble:::.data.route(site, save.dir = save.dir))
        )
        int.data$startDateTime=as.POSIXct(int.data$startDateTime, tz="UTC")#-lubridate::hours(offset+2)

        int.rad<-data.frame(int.data %>%
                                group_by(startDateTime = cut(startDateTime, breaks="60 min")) %>%
                                summarize(gloRadMean.000.060 = mean(gloRadMean.000.060)))
        int.rad$startDateTime=as.POSIXct(int.rad$startDateTime, tz="UTC")

        all.data=merge(x=int.rad, y=ext.data, by.y = "Date", by.x = "startDateTime")

        #qplot(x=all.data$Solar.Radiation.Average..watt.m2., y=all.data$gloRadMean.000.060)
        ext.consist=cor.test(x=all.data$Solar.Radiation.Average..watt.m2., y = all.data$gloRadMean.000.060, conf.level = 0.95, method = "spearman", exact = F)$estimate


    }else{

        uscrn.site=as.character(Noble::rad_dq_info$nearestUSCRN[Noble::rad_dq_info$Site==site])
        temp=Noble::pull.USCRN.data(timeScale = "subhourly",
                                    stationID = uscrn.site,
                                    TimeBgn = paste0(bgn.month, "-01"),
                                    TimeEnd =  as.character(Noble::end.day.time(end.month = end.month, time.agr = 1))
        )
        ext.data=temp
        write.csv(x = ext.data, file = paste0(raw.dir, "USCRN_", uscrn.site, ".csv"), row.names = F)

        int.data=try(Noble::data.pull(site = site,
                                      dpID = "DP1.00014.001",
                                      bgn.month = bgn.month,
                                      end.month = end.month,
                                      time.agr = 30,
                                      package = "basic",
                                      save.dir = Noble:::.data.route(site, save.dir = save.dir))
        )

        extRad<-data.frame(ext.data %>%
                               group_by(UTC_DATE = cut(UTC_DATE, breaks="30 min")) %>%
                               summarize(SOLAR_RADIATION = mean(SOLAR_RADIATION)))

        bothRad<-data.frame(cbind(extRad, int.data[,grepl(x = colnames(int.data), pattern = "gloRadMean")]))
        colnames(bothRad)<-c("UTC_Date", "ExtRad", "NEONRad")
        spearman.results=cor.test(bothRad$ExtRad, bothRad$NEONRad, method = "spearman", conf.level = 0.95, exact = F)
        ext.consist=spearman.results$estimate
        write.csv(x = data.frame(unlist(spearman.results)), file = paste0(raw.dir, "external_comparison.csv"), row.names = T)
    }
    if(ext.consist>0.95){external.test="Pass"}else{external.test="Fail"}
    if(interactive()){
        message(paste0("External Consistency Test: ", external.test))
        }
}
