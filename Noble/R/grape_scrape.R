############################################################################################
# title  Copies grape logs from NEON Engineering to a specified local directory

# author Robert Lee \email{rlee@battelleecology.org}\cr

# description Copies grape log files from Common/ENG/Sites/Sensors/data/ on the NEON internal server
# to a specified directory.
# Only logs from a specified site and within the specified date range are coppied to the local directory
#
#
# param \code{bgn.date} The fisrt date for grape logs to be copied (as YYYYMMDD).
# param \code{end.date} The last date for grape logs to be copied (as YYYYMMDD).
# param \code{system} The type of instrumented system to copy logs for, either TIS or AIS.
# param \code{test.sites} A list of 4-letter NEON site codes representing the sites to copy grape data for.
# param \code{save.dir} The final save directory of the logs.
#
# return A time sequence over the specified interval.
#
#
# keywords process quality, data quality, gaps, commissioning

# examples
#

# seealso Currently none

# changelog and author contributions / copyrights
#   Robert Lee (2017-07-18)
#     original creation
#
##############################################################################################

.grape.scrape=function(bgn.date, end.date, system, test.sites, save.dir){
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
        site.meta =Noble::tis_site_config
       #site.meta<-data.frame(read.csv(paste0(comm.dir, "TIS_site_config.csv"), header = T))
        site.list<-site.meta$SiteID
    }else if(system=="AIS"){
        test.dir<-paste0(comm.dir,"AisGrapeFaultRateSystemPerformance/")
        site.meta<-data.frame(read.csv(paste0(comm.dir, "AIS_site_config.csv"), header = T))
        site.list<-site.meta$SiteID
    }


    found.sites<-list.files(log.dir)[list.files(log.dir) %in% site.list]
    test.sites<-test.sites[test.sites %in% found.sites]

    test.log.dirs<-paste0(log.dir, test.sites, "/")

    # Subset out the files from the log directory that are between the dates listed, and copy them into the test directory
    # NOTE: The directory is cleaned out before files are copied over- be careful with which sites you need!

    list.test.dirs<-c()
    for(i in 1:length(test.log.dirs)){
        #print(i)
        message(paste0("Copying Grape logs for ", test.sites[i], "."))
        temp.logs<-list.files(test.log.dirs[i])
        temp.log.name<-strsplit(temp.logs, split = "\\_")
        temp.date.indx<-c()

        temp.domn<-nneo::nneo_site(test.sites[i])$domainCode

        for(j in 1:length(temp.logs)){
            if(dplyr::between(x=as.Date(temp.log.name[[j]][2], format="%Y%m%d"), left=bgn.date, right=end.date)){
                temp.date.indx<-append(temp.date.indx, j)
            }
        }

        trans.files<-paste0(test.log.dirs[i], temp.logs[temp.date.indx])
        dest.dir<-paste0(save.dir,"/", temp.domn, "-", test.sites[i])
        if(!dir.exists(dest.dir)){dir.create(dest.dir)}
        clean.up<-list.files(dest.dir)
        if(!length(clean.up)==0){
            file.remove(paste0(dest.dir, "/", clean.up))
        }
        #print(dest.dir)
        file.copy(from=trans.files, to=dest.dir)
        list.test.dirs<-append(list.test.dirs, dest.dir)
    }
    message(paste0("Files copied to ", save.dir))
    return(list.test.dirs)
}
