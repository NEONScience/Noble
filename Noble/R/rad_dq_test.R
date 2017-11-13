#Test Block
site="HARV"

bgn.month="2017-06"
end.month="2017-07"

test = "TIS Radiation Data Quality "
testSubDir = "TisRadiationDataQuality"
if(grepl("darwin", version$os))
{
    mountPoint<-"/Volumes/neon/" #Mac
}else{
    mountPoint<-"N:/" #Windows
}
dirCommBase = paste0(mountPoint, "Science/Science Commissioning Archive/SiteAndPayload/")
testFullDir=paste0(dirCommBase, testSubDir, "/")
save.dir=testFullDir





## Function start
rad.dq.test=function(site, save.dir, bgn.month, end.month){
    ########### GENERAL PARAMETERS ###########
    #Define directories
    domn=Noble::is_site_config$Domain[Noble::is_site_config$SiteID==site]
    site.dir=paste0(save.dir, "/", domn, "-", site, "/")

    rslt.dir=paste0(save.dir, "/", "Common/")
    if(!dir.exists(rslt.dir)){
        dir.create(rslt.dir)
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
    frst.week=c(as.Date(paste0(bgn.month, "-01")), day1.week1+7)
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


    ########### INTERNAL CONSISTENCY ###########
    ########### PAR and QL PAR ONLY ############
    site.MLs=1:Noble::tis_site_config$Num.of.MLs[Noble::tis_site_config$SiteID==site]
    if(Noble::rad_dq_info$classification[Noble::rad_dq_info$site==site]=="forest"){rho.TH=.65}else{rho.TH=.9}

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

    PAR.rho=unlist(lapply(PAR.pairwise, function(x) cor.test(PAR[,x[1]], PAR[,x[2]], method = "spearman")$estimate))
    QL.PAR.rho= unlist(lapply(QL.PAR.pairwise, function(x) cor.test(QL.PAR[,x[1]], QL.PAR[,x[2]], method = "spearman")$estimate))

    names(PAR.rho)=paste0("PAR-", PAR.pairwise)
    names(QL.PAR.rho)=c("QL PAR 1-3", "QL PAR 3-5")

    if(any(PAR.rho<rho.TH)==F){PAR.rho.test="Pass"}else{PAR.rho.test="Fail"} # ------>PAR rho results####
    if(any(QL.PAR.rho<rho.TH)==F){QL.PAR.rho.test="Pass"}else{QL.PAR.rho.test="Fail"} # ------>QL PAR rho results####
    raw.stats=data.frame(rho.estimate=append(PAR.rho, QL.PAR.rho))

    ########## WRITE TO RAW STATS FILE ##########

    write.csv(x = raw.stats, file = paste(site.dir,"rawStats.csv",sep = "/"), col.names = T, row.names = F)

    ########### TOWER-TOP CONSISTENCY ###########
    ########### Direct & Diffuse ONLY ###########

    DirDif=try(Noble::data.pull(site = site,
                                dpID = "DP1.00014.001",
                                bgn.month = bgn.month,
                                end.month = end.month,
                                time.agr = 30,
                                package = "basic",
                                save.dir = site.dir)
    )

    DirDif=data.frame(startDateTime=DirDif[,1],
                      dirRadMean=DirDif[,grepl(colnames(DirDif), pattern = "dirRadMean")],
                      difRadMean=DirDif[,grepl(colnames(DirDif), pattern = "difRadMean")],
                      gloRadMean=DirDif[,grepl(colnames(DirDif), pattern = "gloRadMean")]
    )

    # Sum up the direct and diffuse rad
    DirDif$total=DirDif$dirRadMean+DirDif$difRadMean

    # Convert to local time
    time.zone=Noble::tis_site_config$Time.Zone[Noble::tis_site_config$SiteID==site]
    DirDif$startDateTime=as.POSIXct(DirDif$startDateTime, tz="UTC")
    DirDif$startDateTime=as.POSIXct(format(DirDif$startDateTime, tz=time.zone, usetz = T), tz=time.zone, usetz = T)

    #Only rows with greater than 5 W/m^2 get tested
    DirDif=DirDif[which(DirDif$total>50),]

    DirDif$SZA=RAtmosphere::SZA(timein = DirDif$startDateTime,
                                Lat = Noble::tis_site_config$Latitude[Noble::tis_site_config$SiteID==site],
                                Lon = Noble::tis_site_config$Longitude[Noble::tis_site_config$SiteID==site]
    )

    #Ratio is within ±8% for solar zenith angle < 75°
    set1=DirDif[which(DirDif$SZA<75),]
    ratio1=sum(DirDif$total)/sum(DirDif$gloRadMean)

    # b. Ratio is within ±15% for 93° > solar zenith angle > 75°
    set2=DirDif[which(DirDif$SZA>75&&DirDif$SZA<93),]





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
