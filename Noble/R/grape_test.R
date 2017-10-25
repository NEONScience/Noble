## TEST BLOCK #####
# end.date<-"20170630"
# bgn.date<-"20170330"
# system<-"TIS"
#
# test.sites<-c("LENO", "RMNP")

#require dates in YYYYMMDD format
#' @export
#FUCNTION START#####
.grape.test<-function(bgn.date, end.date, system, test.sites, num.grapes){

    bgn.date<-as.Date(bgn.date, format="%Y%m%d")
    end.date<-as.Date(end.date, format="%Y%m%d")

    #Specify system type for the test
    ctrl.syst<-c("TIS", "AIS")
    if(missing(system)|!system %in% ctrl.syst){stop("Please specify 'TIS' or 'AIS' for the system parameter.")}

    if(missing(test.sites)){test.sites<-""}

    #Routing of external drives based on OS:
    if(grepl("darwin", version$os))
    {
        mount.point<-"/Volumes/neon/" #Mac
    }else{
        mount.point<-"N:/" #Windows
    }

    #Set some base directories
    log.dir<-paste0(mount.point, "Common/ENG/Sites/Sensors/data/")
    comm.dir<-paste0(mount.point, "Science/Science Commissioning Archive/SiteAndPayload/")

    #accomodate both system types
    if(system=="TIS"){
        test.dir<-paste0(comm.dir,"TisGrapeFaultRateSystemPerformance/")
        site.meta<-data.frame(read.csv(paste0(comm.dir, "TIS_site_config.csv"), header = T))
        site.list<-site.meta$SiteID
    }else if(system=="AIS"){
        test.dir<-paste0(comm.dir,"AisGrapeFaultRateSystemPerformance/")
        site.meta<-data.frame(read.csv(paste0(comm.dir, "AIS_site_config.csv"), header = T))
        site.list<-site.meta$SiteID
    }


    found.sites<-list.files(log.dir)[list.files(log.dir) %in% site.list]
    test.sites<-test.sites[test.sites %in% found.sites]

    test.log.dirs<-paste0(log.dir, test.sites, "/")

    #THIS BIT DOES ALL THE GRAPE COPYING
    list.test.dirs=Noble:::.grape.scrape(bgn.date=bgn.date, end.date=end.date, system=system, test.sites=test.sites, save.dir=test.dir)

    #THIS LINE MAKES THE LIST OF FOUND GRAPES
    Noble:::.grape.list(list.test.dirs=list.test.dirs)

    for(l in 1:length(list.test.dirs)){
        test.files<-list.files(list.test.dirs[l])
        test.files<-test.files[-which(grepl(pattern = "-Grapes.csv", x = test.files, ignore.case = T))]

        uptime<-c()

        for(q in 1:length(test.files)){
            meta<-strsplit(test.files[q], split = "_")
            site<-unlist(meta[[1]][1])
            date<-as.POSIXct(unlist(meta[[1]][2]), format ="%Y%m%d")-(3600*24)
            tempCSV<-read.csv(file=paste0(list.test.dirs[l], "/", test.files[q]))

            tempCSV<-read.csv(file=paste0(list.test.dirs[l], "/", test.files[q]))
            uptime<-append(uptime, round(sum(tempCSV[which(grepl(pattern = "7CE*", x=tempCSV[,2])),4]), digits = 2))
        }

        siteGrapes<-read.csv(paste0(list.test.dirs[l], "/", site, "-Grapes.csv"))

        grape.df=Noble:::.grape.df(site=site, log.dir = test.dir, system = system)
        if(missing(num.grapes)){
            num.grapes=max((colSums(!is.na(grape.df[,2:length(colnames(grape.df))]))))
        }
        pcntUp<-round(((mean(uptime)/(num.grapes))*100), digits = 1)

        frstFile<-strsplit(test.files[1], split = "_")
        lastFile<-strsplit(test.files[length(test.files)], split= "_")
        startDate<-as.Date.character(unlist(frstFile[[1]][2]), format="%Y%m%d")
        endDate<-as.Date.character(unlist(lastFile[[1]][2]), format="%Y%m%d")
        resultString<-paste0(site, ", Uptime: ", round(pcntUp, digits = 2), ", Start: ", startDate, ", End: ", endDate, ", Total Days: ", q, ", total grapes found: ", num.grapes)

        if(pcntUp>=99){final.result<-"Pass"}else{final.result<-"Fail"}



        if(file.exists(paste0(test.dir, "Common/results.csv"))){


            rslts<-read.csv(paste0(test.dir, "Common/results.csv"))
            rslt.DF<-data.frame(Site=site, Uptime=pcntUp, Start.Date = as.character(startDate), End.Date=as.character(endDate), Test.Date = as.character(as.Date(Sys.Date())), Total.Days=length(test.files), Grapes.Found = num.grapes, Result=final.result)



            if(site %in% rslts$Site){
                rslts[rslts$Site==site,]<-rslt.DF
            }else{
                rslts<-rbind(rslts, rslt.DF)
            }

            write.csv(rslts, file = paste0(test.dir, "Common/results.csv"), row.names = F)

        }else{

            rslt.DF<-data.frame(Site=site, Uptime=pcntUp, Start.Date = as.character(startDate), End.Date=as.character(endDate), Test.Date = as.character(as.Date(Sys.Date())), Total.Days=length(test.files), Grapes.Found = num.grapes, Result=final.result)

            write.csv(rslt.DF, file = paste0(test.dir, "Common/results.csv"), row.names = F)

        }
        print(rslt.DF)
    }

}

