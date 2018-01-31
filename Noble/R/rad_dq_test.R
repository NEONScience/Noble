############################################################################################
#' @title  Execute Radiation Data Quality Testing

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description Run 4 checks on radiation data quality for TIS sites: (1) Internal Correlation,
#' (2) Tower-Top Sensor Agreement, (3) Variance Stability, and
#' (4) External Correlation with a Non-NEON Site. Test results are written as simple pass/fail entries
#' in the results.csv files, however expanded test resullts are written in a site-specific folder in
#' the save directory.
#'
#' @param \code{site} The TIS site of interest, as a 4-letter code.
#' @param \code{save.dir} The save directory for data, results, and other files.
#' @param \code{bgn.month} The first month of testing, as "YYYY-MM".
#' @param \code{end.month} The last month of testing, as "YYYY-MM".
#'
#' @return Site results in 'results.csv', and raw

#' @keywords process quality, data quality, gaps, commissioning

#' @examples
#' \dontrun{
#' site="BART"
#' bgn.month="2017-06"
#' end.month="2017-07"
#'
#' testSubDir = ""
#' if(grepl("darwin", version$os))
#' {
#'     mountPoint<-"/Volumes/neon/" #Mac
#' }else{
#'     mountPoint<-"N:/" #Windows
#' }
#' testFullDir =  paste0(mountPoint, "Science/Science Commissioning Archive/SiteAndPayload/TisRadiationDataQuality/")

#' rad.dq.test(site=site, save.dir=testFullDir, bgn.month=bgn.month, end.month=end.month)
#' }

#'
#' @seealso Currently none

# changelog and author contributions / copyrights
#   Robert Lee (2018-01-03)
#     Re-wrote from earlier script (original creation)
#
##############################################################################################


## Function start
rad.dq.test=function(site, save.dir, bgn.month, end.month){

    ########### GENERAL PARAMETERS ###########
    if(!(site %in% Noble::rad_dq_info$Site)){
        message("Site is not in the current DQ test list. Please select from:")
        stop(paste0(Noble::rad_dq_info$Site))
    }
    message(paste0("Testing ", site))

    #Define directories
    domn=Noble::is_site_config$Domain[Noble::is_site_config$SiteID==site]
    site.dir=Noble:::.data.route(site=site, save.dir=save.dir)

    rslt.dir=paste0(save.dir, "/Common/")
    if(!dir.exists(rslt.dir)){
        dir.create(rslt.dir)
    }

    raw.dir=paste0(site.dir, "/rawData/")
    if(!dir.exists(raw.dir)){
        dir.create(raw.dir)
    }

    #set up DP info
    test.dpIDs=c("DP1.00014.001",
                 "DP1.00023.001",
                 "DP1.00024.001",
                 "DP1.00066.001"
    )
    # Add in Primary Pyranometer if Core site
    if(Noble::tis_site_config$Core.Relocatable[Noble::tis_site_config$SiteID==site]=="Core"){test.dpIDs=c(test.dpIDs, "DP1.00022.001")}



    ########### VARIANCE TESTING ###########
    ######## ALL AVAILABLE RAD DATA ########

    # First week start
    frst.week=c(as.Date(paste0(bgn.month, "-01")), as.Date(paste0(bgn.month, "-01"))+7)
    last.week=c(as.Date(Noble::end.day.time(end.month = end.month, time.agr = 1))-7, as.Date(Noble::end.day.time(end.month = end.month, time.agr = 1)))

    # Pull and refine data to vairance fields only
    raw.var.data=lapply(test.dpIDs, function(x)
        try(Noble::data.pull(site = site,
                             dpID = x,
                             bgn.month = bgn.month,
                             end.month = end.month,
                             time.agr = 30,
                             package = "basic",
                             save.dir = site.dir)
        )
    )
    #Put into massive data frame
    var.data=do.call(cbind, raw.var.data)
    var.data=data.frame(startDateTime=var.data[,1], var.data[,grepl(pattern = "variance", x = colnames(var.data), ignore.case = T)])
    var.data=var.data[,-which(grepl(pattern = "*LW*", x = colnames(var.data)))]

    # Convert to local time
    time.zone=Noble::tis_site_config$Time.Zone[Noble::tis_site_config$SiteID==site]
    var.data$startDateTime=as.POSIXct(var.data$startDateTime, tz="UTC")
    var.data$startDateTime=as.POSIXct(format(var.data$startDateTime, tz=time.zone, usetz = T), tz=time.zone, usetz = T)

    #subset to first and last weeks
    first.pop=var.data[frst.week[2]>var.data$startDateTime&var.data$startDateTime>=frst.week[1],]
    last.pop=var.data[last.week[2]>var.data$startDateTime&var.data$startDateTime>=last.week[1],]

    #Subset to nightime conditions
    test.time = c("00:00:00", "00:30:00", "01:00:00", "01:30:00", "02:00:00", "02:30:00", "03:00:00",
                  "03:30:00", "04:00:00")
    first.pop=first.pop[which(strftime(first.pop$startDateTime, format="%H:%M:%S", tz=time.zone) %in% test.time),]
    last.pop=last.pop[which(strftime(last.pop$startDateTime, format="%H:%M:%S", tz=time.zone) %in% test.time),]

    ## Remove all NAs, do this after subsetting to nightime conditions
    first.pop=first.pop[,-which(colSums(is.na(first.pop))==length(first.pop[,1]))]
    last.pop=last.pop[,-which(colSums(is.na(last.pop))==length(last.pop[,1]))]

    f.test=stats::var.test(unlist(as.list(first.pop[,(2:length(colnames(first.pop)))])),# ------>f.test results####
                           unlist(as.list(last.pop[,(2:length(colnames(last.pop)))])))

    if(f.test$statistic>1.05|f.test$statistic<0.95){f.test.result="Fail"}else{f.test.result="Pass"} ################################################################

    message(paste0("Variance Stability Test: ", f.test.result))
    write.csv(x = data.frame(value=unlist(f.test)),file = paste0(raw.dir, "variance_stats.csv"))

    ########### INTERNAL CONSISTENCY ###########
    ########### PAR and QL PAR ONLY ############
    site.MLs=1:Noble::tis_site_config$Num.of.MLs[Noble::tis_site_config$SiteID==site]
    if(Noble::rad_dq_info$classification[Noble::rad_dq_info$Site==site]=="forest"){rho.TH=.65}else{rho.TH=.9}

    PAR.pairwise=lapply(site.MLs[-1], function(x) c(x-1, x))
    QL.PAR.pairwise=list(c(1,2), c(2,3))

    PAR=try(Noble::data.pull(site = site,
                             dpID = "DP1.00024.001",
                             bgn.month = bgn.month,
                             end.month = end.month,
                             time.agr = 30,
                             package = "basic",
                             save.dir = site.dir)
    )
    QL.PAR=try(Noble::data.pull(site = site,
                                dpID = "DP1.00066.001",
                                bgn.month = bgn.month,
                                end.month = end.month,
                                time.agr = 30,
                                package = "basic",
                                save.dir = site.dir)
    )

    PAR=PAR[,grepl(pattern = "^PARMean", x = colnames(PAR))]
    QL.PAR=QL.PAR[,grepl(pattern = "linePARMean", x = colnames(QL.PAR))]

    PAR.rho=unlist(lapply(PAR.pairwise, function(x) cor.test(PAR[,x[1]], PAR[,x[2]], method = "spearman", exact = F)$estimate))
    QL.PAR.rho= unlist(lapply(QL.PAR.pairwise, function(x) cor.test(QL.PAR[,x[1]], QL.PAR[,x[2]], method = "spearman", exact = F)$estimate))

    names(PAR.rho)=paste0("PAR-", PAR.pairwise)
    names(QL.PAR.rho)=c("QL PAR 1-3", "QL PAR 3-5")

    if(any(PAR.rho<rho.TH)==F){PAR.rho.test="Pass"}else{PAR.rho.test="Fail"} # ------>PAR rho results####
    if(any(QL.PAR.rho<rho.TH)==F){QL.PAR.rho.test="Pass"}else{QL.PAR.rho.test="Fail"} # ------>QL PAR rho results####
    ### Write stats out ###
    raw.stats=data.frame(rho.estimate=append(PAR.rho, QL.PAR.rho))
    write.csv(x = raw.stats, file = paste0(raw.dir,"par_rho_stats.csv"), row.names = T)


    ### Final Results ###
    if(PAR.rho.test=="Fail"|QL.PAR.rho.test=="Fail"){
        internal.compair.result="Fail"
    }else{internal.compair.result="Pass"}

    message(
        paste0("Internal Comparison Test: ", internal.compair.result) ###############################################################################################
    )

    ########### TOWER-TOP CONSISTENCY ###########
    ########### Direct & Diffuse ONLY ###########

    DirDif=try(Noble::data.pull(site = site,
                                dpID = "DP1.00014.001",
                                bgn.month = bgn.month,
                                end.month = end.month,
                                time.agr = 30,
                                package = "basic",
                                save.dir = Noble:::.data.route(site, save.dir = save.dir))
    )

    DirDif=data.frame(startDateTime=DirDif[,1],
                      dirRadMean=DirDif[,grepl(colnames(DirDif), pattern = "dirRadMean")],
                      difRadMean=DirDif[,grepl(colnames(DirDif), pattern = "difRadMean")],
                      gloRadMean=DirDif[,grepl(colnames(DirDif), pattern = "gloRadMean")]
    )



    # Convert to local time
    time.zone=Noble::tis_site_config$Time.Zone[Noble::tis_site_config$SiteID==site]
    DirDif$startDateTime=as.POSIXct(DirDif$startDateTime, tz="UTC")
    DirDif$startDateTime=as.POSIXct(format(DirDif$startDateTime, tz=time.zone, usetz = T), tz=time.zone, usetz = T)

    #Only rows with greater than 5 W/m^2 get tested


    DirDif$SZA=RAtmosphere::SZA(timein = DirDif$startDateTime,
                                Lat = Noble::tis_site_config$Latitude[Noble::tis_site_config$SiteID==site],
                                Lon = Noble::tis_site_config$Longitude[Noble::tis_site_config$SiteID==site]
    )

    # Sum up the direct and diffuse rad
    DirDif$total=DirDif$dirRadMean*cos(DirDif$SZA/180*pi)+DirDif$difRadMean
    DirDif=DirDif[DirDif$total>50&!is.na(DirDif$total),]

    # A. Ratio is within ±8% for solar zenith angle < 75°
    set1=DirDif[which(DirDif$SZA<75),]
    ratio1=sum(set1$total)/sum(set1$gloRadMean)
    if(0.92<ratio1&ratio1<1.08){
        rat1Result="Pass"
    }else(rat1Result="Fail")

    # B. Ratio is within ±15% for 93° > solar zenith angle > 75°
    set2=DirDif[which(DirDif$SZA>75&&DirDif$SZA<93),]
    ratio2="NA"
    rat2Result="NA"
    if(length(set2[,1]>0)){
        ratio2=sum(set2$total)/sum(set2$gloRadMean)
        if(0.85<ratio1&ratio1<1.15){
            rat2Result="Pass"
        }else(rat2Result="Fail")
    }
    if(rat2Result=="Fail"|rat1Result=="Fail"){
        ratio.result="Fail"
    }else{ratio.result="Pass"}

    ### Raw data out
    tower.top=data.frame(sets=c("SZA<75", "75<SZA<93"), ratio=c(ratio1, ratio2), result=c(rat1Result, rat2Result))
    write.csv(x = tower.top, file = paste0(raw.dir,"tower_top_ratios.csv"), row.names = F)

    message(
        paste0("Tower Top Consistency Test: ", ratio.result) ##############################################################################################################
    )

    ########### EXTERNAL CONSISTENCY ###########
    ######## ALL GLORAD DATA ########

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

    ext.data$UTC_DATE=as.POSIXct(ext.data$UTC_DATE, tz="UTC")-lubridate::minutes(30)

        int.data=try(Noble::data.pull(site = site,
                                      dpID = "DP1.00014.001",
                                      bgn.month = bgn.month,
                                      end.month = end.month,
                                      time.agr = 30,
                                      package = "basic",
                                      save.dir = Noble:::.data.route(site, save.dir = save.dir))
        )
        int.data$startDateTime=as.POSIXct(int.data$startDateTime, tz="UTC")
        int.rad=data.frame(startDateTime=int.data$startDateTime, NEON.rad=int.data[,grepl(x = colnames(int.data), pattern = "gloRadMean")])
        ext.rad<-data.frame(ext.data %>%
                               group_by(UTC_DATE = cut(UTC_DATE, breaks="30 min")) %>%
                               summarize(SOLAR_RADIATION = mean(SOLAR_RADIATION)))

        ext.rad$UTC_DATE=as.POSIXct(ext.rad$UTC_DATE, tz="UTC")

        all.data=merge(x=int.rad, y=ext.rad, by.y = "UTC_DATE", by.x = "startDateTime")



        #bothRad<-data.frame(cbind(extRad, int.data[,grepl(x = colnames(int.data), pattern = "gloRadMean")]))

        spearman.results=cor.test(all.data$NEON.rad, all.data$SOLAR_RADIATION, method = "spearman", conf.level = 0.95, exact = F)
        ext.consist=spearman.results$estimate
        write.csv(x = data.frame(unlist(spearman.results)), file = paste0(raw.dir, "external_comparison.csv"), row.names = T)
    }
    if(ext.consist>0.95){external.test="Pass"}else{external.test="Fail"}
    message(
        paste0("External Consistency Test: ", external.test)
    )

    dq.rslt=data.frame(site=site,
                       variance.stability = f.test.result,
                       internal.consistency=internal.compair.result,
                       tower.top.consistency=ratio.result,
                       external.consistency=external.test)

    ########### WRITE TO RESULTS FILE ###########

    if(file.exists(paste(rslt.dir,"results.csv",sep = "/"))){
        dq.rpt <- read.csv(file = paste(rslt.dir,"results.csv",sep = "/"), header = T, stringsAsFactors = T)
        dq.rpt <- rbind(dq.rpt, dq.rslt)
        write.csv(x = dq.rpt, file = paste(rslt.dir,"results.csv",sep = "/"), row.names = F)
    }
    else{
        write.csv(x = dq.rslt, file = paste(rslt.dir,"results.csv",sep = "/"), col.names = T, row.names = F)
    }

}





