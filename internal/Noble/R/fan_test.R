############################################################################################
#' @title  Tests fan aspiration system performance

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description For the specified dates and site, will test and report results in a results.csv file.
#'
#' @param \code{site} Parameter of class character. The NEON site data should be downloaded for.
#' @param \code{bgn.month} Parameter of class character. The year-month (e.g. "2017-01") of the first
#' month to get data for.
#' @param \code{end.month} Parameter of class character. The year-month (e.g. "2017-01") of the last
#'  month to get data for.
#' @param \code{save.dir} Parameter of class character. The local directory where data files should be
#' saved.
#' @param \code{pass.th} Optional, defaults to 95. The percent of data that must be unflagged for the
#' site to pass the test.
#'
#' @return Writes results file to the specified directory.

#' @keywords process quality, data quality, gaps, commissioning

#' @examples Currently none
#' @seealso Currently none

# changelog and author contributions / copyrights
#   Robert Lee (2016-12-12)
#     Original creation
#   Robert Lee (2017-07-24)
#     Updated for Noble
#   Robert Lee (2018-01-12)
#     Remove nneo code
##############################################################################################

fan.test<- function(site=site, bgn.month, end.month, save.dir, pass.th=95){

    time.agr=30
    bgn_temp <- as.Date(paste0(bgn.month, "-01"), tz="UTC")
    end_temp <- as.Date(paste0(end.month, "-01"), tz="UTC")
    bgn_temp <- as.POSIXct(paste0(bgn.month, "-01"), tz="UTC")
    end_temp<- as.POSIXlt(paste0(end_temp, "-01"), tz="UTC")
    end_temp$mon<-end_temp$mon+1
    end_temp<-end_temp-lubridate::minutes(time.agr)-lubridate::seconds(1)

    if(missing(pass.th)){
        pass.th=95
    }

    days=difftime(end_temp, bgn_temp, units = "days")

    siteCfig <- Noble::tis_site_config
    #### Commissioning Test Parameters ####


    save.dir<- save.dir #"/Volumes/neon/Science/Science Commissioning Archive/SiteAndPayload/TisFanAspirationSystemPerformance/Common/"
    #### Other Parameters ####
    package <- "expanded"
    Kpi <- "Air Temp"
    domn= Noble::tis_site_config$Domain[Noble::tis_site_config$SiteID==site]

    dat_dir=Noble:::.data.route(site = site, save.dir = save.dir)

    SAATnumber <- "DP1.00002.001"
    TAATnumber <- "DP1.00003.001"

    #get the number of measurement levels at the site
    siteCfigIndx <- grep(siteCfig$SiteID, pattern=site)
    mlSAAT <- unlist(siteCfig[siteCfigIndx,SAATnumber])
    mlTAAT <- mlSAAT+1

    #### Expected files for the site and times specified ####

    SAATfile <- paste0(dat_dir, "NEON.", domn,".", site,".DP1.00002.001", "_REQ_",as.Date(bgn_temp),"_",as.character(as.Date(end_temp)),"_",time.agr,"min_",package,".csv",".gz")
    TAATfile <- paste0(dat_dir, "NEON.", domn,".", site,".DP1.00003.001", "_REQ_",as.Date(bgn_temp),"_",as.character(as.Date(end_temp)),"_",time.agr,"min_",package,".csv",".gz")

    # Don't download if the file matching specifications exists in the SCA
    if(!file.exists(SAATfile)){

        sink<-Noble::data.pull(site = site, dpID = SAATnumber, bgn.month = bgn.month, end.month = end.month, time.agr = time.agr, package=package, save.dir=dat_dir)
        rm(sink)

    }

    if(!file.exists(TAATfile)){

        sink<-Noble::data.pull(site = site, dpID = TAATnumber, bgn.month = bgn.month, end.month = end.month, time.agr = time.agr, package=package, save.dir=dat_dir)
        rm(sink)
    }

    #### Load the files ####

    SAATData<- as.data.frame(read.csv(SAATfile, header = T))
    TAATData<- as.data.frame(read.csv(TAATfile, header = T))

    #### Scrape relevant Data ####
    SAATFlowIndx <- grep(pattern = "*flow", colnames(SAATData), ignore.case = T)
    SAATHeatIndx <- grep("*heat", colnames(SAATData), ignore.case = T)
    SAATDataIndx <- grep("tempSinglemean", colnames(SAATData), ignore.case = T)

    TAATFlowIndx <- grep("*flow", colnames(TAATData), ignore.case = T)
    TAATHeatIndx <- grep("*heat", colnames(TAATData), ignore.case = T)
    TAATDataIndx <- grep("temptriplemean", colnames(TAATData), ignore.case = T)

    #### Store the scraped data ####
    fanAspData <- as.data.frame(SAATData[,1])

    fanAspData <-cbind(fanAspData, SAATData[SAATDataIndx])
    fanAspData <-cbind(fanAspData, SAATData[SAATHeatIndx])
    fanAspData <-cbind(fanAspData, SAATData[SAATFlowIndx])

    fanAspData <-cbind(fanAspData, TAATData[TAATDataIndx])
    fanAspData <-cbind(fanAspData, TAATData[TAATHeatIndx])
    fanAspData <-cbind(fanAspData, TAATData[TAATFlowIndx])

    #### Re-index new data frame for correct variables ####
    allDataIndx <- grep(pattern = "*mean", colnames(fanAspData), ignore.case = T)
    heatIndx <- grep(pattern = "*heaterPass", colnames(fanAspData), ignore.case = T)
    flowIndx <- grep(pattern = "*flowPass", colnames(fanAspData), ignore.case = T)

    flowPass<-colSums(fanAspData[flowIndx], na.rm = T)
    nObs <- 0

    for(i in 1:length(flowIndx)){
        temp<-na.omit(fanAspData[flowIndx[i]])
        nObs<-(nObs+length(temp[,1]))
    }
    #pcntFlowPass <- round(sum(flowPass)/(length(fanAspData$`SAATData$POSIXseq`)*mlTAAT), digits = 2)
    # ommmit gapped data, only test the percent passing where data exists:
    pcntFlowPass <- round(sum(flowPass)/(nObs), digits = 2)
    print(pcntFlowPass)

    dataRpt = data.frame(site=site,
               time_performed=as.character(Sys.time()),
               begin_month=bgn.month,
               end_month=end.month,
               days_tested=days,
               percent_pass=pcntFlowPass,
               pass_threshold=pass.th,
               user_remark=""
    )

    rslt.dir=paste0(save.dir, "/Common/")
    if(!dir.exists(rslt.dir)){
        dir.create(rslt.dir)
    }
    ####------####
    if (file.exists(paste(rslt.dir, "results.csv", sep = ""))) {
        temp=read.csv(file = paste0(rslt.dir, "results.csv"))
        dataRpt=rbind(temp, dataRpt)
        write.csv(x = dataRpt, file = paste0(rslt.dir, "results.csv"), row.names = F)
    }
    else {
        write.csv(x = dataRpt, file = paste0(rslt.dir, "results.csv"), row.names = F)
    }
    ####------####
}

