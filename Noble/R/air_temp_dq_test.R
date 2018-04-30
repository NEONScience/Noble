############################################################################################
#' @title  Test NEON Air Temperature data for Stability of Variance, Internal Consistency,
#' and External Consistency.

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description For the specified dates and site, the function will perform variance stability testing
#'  on overnight data (midnight to 4:00 local time), internal consistency checks on adjacent
#'  measurement locations, and external consistency checks with the closest NRCS or USCRN site.
#'
#'
#' @param \code{site} Parameter of class character. The NEON site data should be downloaded for.
#' @param \code{dpID} Parameter of class character. The data product code in question. See
#' \code{Noble::tis_pri_vars} for a selected list of data product names and codes, or
#' \url{http://data.neonscience.org/data-product-catalog} for a complete list.
#' @param \code{bgn.month} Parameter of class character. The year-month (e.g. "2017-01") of the first month to get data for.
#' @param \code{end.month} Parameter of class character. The year-month (e.g. "2017-01") of the last month to get data for.
#' @param \code{time.agr} Parameter of class numeric. The data agregation interval requested, must be 1, 2, 5, or 30.
#' @param \code{package} Parameter of class character. Optional. The type of data package to be returned If not specified, defaults to basic.
#' @param \code{save.dir} Parameter of class character. The local directory where data files should be saved.
#'
#' @return Writes data files to the specified directory.

#' @keywords process quality, data quality, gaps, commissioning

#' #Make a temporary direcotry for the example:
#' tempDir<- tempdir()
#' data.pull(site = "CPER", dpID = "DP1.00002.001", bgn.month = "2017-04", end.month = "2017-05", time.agr = 30, package="basic", save.dir= tempDir)

#' @seealso Currently none

# changelog and author contributions / copyrights
#   Robert Lee (2017-07-18)
#     original creation
#   Robert Lee (2018-04-24)
#     Function refinement
##############################################################################################

# bgn.month="2018-01"
# end.month="2018-02"
# site="MOAB"
# save.dir="/Volumes/neon/Science/Science Commissioning Archive/SiteAndPayload/TisAirTempDataQuality/"

air.temp.dq.test<-function(site, bgn.month, end.month, save.dir){
    ## PART 1: Variance Stability
    saat.test.data=Noble::data.pull(site = site, dpID = "DP1.00002.001", bgn.month = bgn.month, end.month = end.month, time.agr = 1, package = "basic", save.dir = save.dir)
    taat.test.data=Noble::data.pull(site = site, dpID = "DP1.00003.001", bgn.month = bgn.month, end.month = end.month, time.agr = 1, package = "basic", save.dir = save.dir)

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
    group.one=data.frame(startDateTime=group.one$startDateTime, group.one[,grepl(pattern = "tempSingleVariance", x = colnames(group.one))|grepl(pattern = "tempTripleVariance", x = colnames(group.one))])
    group.one=group.one[lubridate::hour(group.one$startDateTime) %in% c(0:5),]
    group.two=Noble::date.extract(data = test.data, bgn.date = end_temp-lubridate::days(15), end.date = end_temp-lubridate::minutes(30)-lubridate::seconds(1))
    group.two=data.frame(startDateTime=group.two$startDateTime, group.two[,grepl(pattern = "tempSingleVariance", x = colnames(group.two))|grepl(pattern = "tempTripleVariance", x = colnames(group.two))])
    group.two=group.two[lubridate::hour(group.two$startDateTime) %in% c(0:5),]

    f.test=c()
    for(i in 2:length(colnames(group.one))){
    f.test=append(f.test, var.test(x=group.one[,i], y = group.two[,i], ratio = 1, conf.level = 0.95)$estimate)
    }
    mean=mean(f.test, na.rm = T)
    f.test=append(f.test, c("mean"=mean))
    variance=data.frame(ML=c(seq(Noble::tis_site_config$Num.of.MLs[Noble::tis_site_config$SiteID==site]), "Mean"), f.test)

    #f.test=var.test(x=unlist(as.list(group.one[2:length(group.one)])), y=unlist(as.list(group.two[2:length(group.two)])), conf.level = 0.99)

    ## PART 2: Internal Consistancy
    internal.data=test.data[,grepl(pattern = "tempSingleMean", x = colnames(test.data))|grepl(pattern = "tempTripleMean", x = colnames(test.data))]
    #internal.data=internal.data[,!(grepl(pattern = "000.010", x=colnames(internal.data)))] #Remove ML1

    spearman=lapply(c( 1:(length(internal.data)-1)), function(l) cor.test(x = internal.data[,l], y = internal.data[,l+1], method = "spearman"))
    mls=unlist(lapply(c( 1:(length(internal.data)-1)), function(x) paste0("ML ", x, "-", x+1)))
    spearman=as.data.frame(do.call(rbind, spearman))
    sman.rho=data.frame(Pair=mls, internal.rho=unlist(spearman$estimate))


    ## PART 3: External Consistancy

    ext.sites=metScanR::getNearby(siteID = paste0("NEON:", site), radius = 20)

    ref.sites=character(0)
    if(length(ext.sites)>0){
        ref.sites=metScanR::getNetwork(ext.sites , network = c("NRCS", "USCRN"))
    }
    rho="NA"
    if(length(ref.sites)>0){
        ext.loc.info=do.call(rbind, lapply(ref.sites, "[[", "location"))


        closest.index=which.min(unlist(lapply(seq(length(ext.loc.info[,1])), function(i)
            geosphere::distm(c(ext.loc.info$longitude_dec[i], ext.loc.info$latitude_dec[i]),
                             c(Noble::tis_site_config$Longitude[Noble::tis_site_config$SiteID==site], Noble::tis_site_config$Latitude[Noble::tis_site_config$SiteID==site])))))

        ref.site=ref.sites[[closest.index]]

        if(ref.site$platform=="USCRN"){
            ref.data=Noble::pull.USCRN.data(timeScale = "subhourly",
                                            stationID = ref.site$identifiers$id[ref.site$identifiers$idType=="WBAN"],
                                            TimeBgn = bgn_temp,
                                            TimeEnd = end_temp,
                                            saveDir = save.dir)
            ref.data$Date=as.POSIXct(ref.data$UTC_DATE, tz="UTC")

            ## Make 30 min direction data from refs
            ref.data=data.frame(ref.data %>%
                                    dplyr::group_by(Date = cut(Date, breaks="30 min")) %>%
                                    dplyr::summarize(airTemp = mean(AIR_TEMPERATURE)))

            neon.data=data.frame(Date=as.POSIXct(saat.test.data$startDateTime, format="%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), neon.temp=saat.test.data$tempSingleMean.000.020)

            ref.data$Date=as.POSIXct(ref.data$Date, tz="UTC")
            neon.data$Date=as.POSIXct(neon.data$Date, tz="UTC")


            comp.data=merge(neon.data, ref.data)
            correlation=cor.test(x=comp.data$neon.temp, y=comp.data$airTemp, method = "spearman", conf.level = 0.95)
            rho=correlation$estimate
        }else if(ref.site$platform=="NRCS"){
            ref.data=RNRCS::grabNRCS.data(network = "SCAN",
                                          site_id = ref.site$identifiers$id[ref.site$identifiers$idType=="SCAN"],
                                          timescale = "hourly",
                                          DayBgn = bgn_temp,
                                          DayEnd = end_temp)

            simple.data=data.frame(
                Date=as.POSIXlt(ref.data$Date, tz=Noble::tis_site_config$Time.Zone[Noble::tis_site_config$SiteID==site]),
                airTemp=ref.data$Air.Temperature.Average..degF.)


            simple.data$Date=format(simple.data$Date, tz="UTC", usetz = T)


            neon.data=data.frame(Date=as.POSIXct(saat.test.data$startDateTime, format="%Y-%m-%d %H:%M:%S", tz = "UTC"), neon.temp=saat.test.data$tempSingleMean.000.020)

            neon.data=data.frame(neon.data %>%
                                     dplyr::group_by(Date = cut(Date, breaks="60 min")) %>%
                                     dplyr::summarize(neon.temp = mean(neon.temp)))
            neon.data$Date=as.POSIXct(neon.data$Date)-lubridate::hours(1) #align with SCAN timing

            simple.data$Date=as.POSIXct(simple.data$Date)

            comp.data=merge(neon.data, simple.data)
            correlation=cor.test(x=comp.data$neon.temp, y=comp.data$airTemp, method = "spearman", conf.level = 0.95)
            rho=correlation$estimate
        }
    }

    out=list("Mean Variance"=variance, "Internal Correlation"=sman.rho, "External Correlation"=rho)
    if(out$`Mean Variance`$f.test[length(out$`Mean Variance`$ML)]>.95&out$`Mean Variance`$f.test[length(out$`Mean Variance`$ML)]<1.05){var.result="Pass"}else{var.result="Fail"}
    if(out$`Internal Correlation`$internal.rho[length(out$`Internal Correlation`$internal.rho)]>.95){int.cor.result="Pass"}else{int.cor.result="Fail"}
    if(out$`External Correlation`>0.95){ext.cor.result="Pass"}else{ext.cor.result="Fail"}
    if(all(c(var.result, int.cor.result, ext.cor.result)=="Pass")){result="Pass"}else{result="Fail"}

    data.dir=Noble:::.data.route(site = site, save.dir = save.dir)
    write.csv(x = out$`Mean Variance`, file = paste0(data.dir, "variance.csv"), row.names = F)
    write.csv(x = out$`Internal Correlation`, file = paste0(data.dir, "internal_comparison.csv"), row.names = F)
    write.csv(x = out$`External Correlation`, file = paste0(data.dir, "external_comparison.csv"), row.names = F)

    rslt.string=

    return(result)
}
