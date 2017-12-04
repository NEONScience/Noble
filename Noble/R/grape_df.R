############################################################################################
#' @title  Generate a Data Frame of all Grape uptimes by Date

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description Private function, which only works with NEON Engineering Grape logs.
#' For all Grapes at a site, a large data frame of uptimes is returned.
#'
#' @param \code{site} A NEON TIS site
#' @param \code{log.dir} The directory storing NEON ENG-formatted sensor log files. If not specified, defaults to grape test directory in the SCA.

#' @return Outputs a data frame of uptimes as a decimal percent, by grape MAC address and date.
#'

#' @keywords process quality, data quality, gaps, commissioning

#' @examples
#'

#' @seealso \function{.grape.scrape}, a function to copy files from the Engineering log directory to a specified output directory.

#' @export

# changelog and author contributions / copyrights
#
#   Robert Lee (2017-07-28)
#     Original creation
#
##############################################################################################

.grape.df<-function(site, log.dir, system){
    if(missing(system)){
        message("Defaulting to TIS")
        system="TIS"
    }

    if(missing(log.dir)){
        #Routing of eternal drives based on OS:
        if(grepl("darwin", version$os))
        {
            mount.point<-"/Volumes/neon/" #Mac
        }else{
            mount.point<-"N:/" #Windows
        }

        comm.dir<-paste0(mount.point, "Science/Science Commissioning Archive/SiteAndPayload/")
        if(system=="TIS"){
            #Set some base directories

            site.meta = Noble::tis_site_config
            test.dir<-paste0(comm.dir,"TisGrapeFaultRateSystemPerformance/")
        }
        if(system=="AIS"){
            site.meta = Noble::ais_site_config
            test.dir<-paste0(comm.dir,"AisGrapeFaultRateSystemPerformance/")
        }

    }else{
        test.dir=log.dir
    }

    if(system=="TIS"){   site.meta = Noble::tis_site_config
    }
    if(system=="AIS"){
        site.meta = Noble::ais_site_config
    }
    #print(system)
    #site.meta<-data.frame(read.csv(paste0(comm.dir, "TIS_site_config.csv"), header = T))
    site.list<-as.character(site.meta$SiteID)
    site.list=append(site.list, as.character(Noble::ais_site_config$SiteID))
    if(!site %in% site.list){stop("'site' is invalid. Please enter a valid TIS site code.")}
    domn<-site.meta$Domain[which(site.meta$SiteID==site)]
    #print(domn)
    site.dir<-paste0(test.dir, "/", domn,"-",site,"/")
    #message(site.dir)
    #if(!dir.exists(site.dir)){stop("Specified log.dir invalid!")}

    grapes<-read.csv(file = paste0(site.dir, site,"-Grapes.csv"))

    grape.logs<-list.files(site.dir)

    grape.logs<-grape.logs[-which(grape.logs==paste0(site, "-Grapes.csv"))]

    grape.df<-data.frame(`Grape MAC Address`=grapes$x)
    for(i in 1:length(grape.logs)){
        #print(i)
        start.col.names<-colnames(grape.df)
        temp.meta<-unlist(strsplit(grape.logs[i], split = "\\_"))
        date<-as.Date(temp.meta[2], format = "%Y%m%d")
        file=paste0(site.dir, grape.logs[i])
        if(file.size(file)>0){
            log.data<-read.csv(file=file)
        }
        found.macs<-trimws(log.data[which(trimws(log.data[,2], "both")==trimws(log.data[,5], "both")),2])
        found.uptimes<-log.data[which(trimws(log.data[,2], "both")==trimws(log.data[,5], "both")),4]

        #grepl(pattern = "7CE*", x=log.data[,2])),2], "both")
        grape.df<-cbind(grape.df, temp.col=rep(NA, length(grape.df$Grape.MAC.Address)))
        for(q in 1:length(found.macs)){
            grape.df$temp.col[which(grape.df$Grape.MAC.Address %in% found.macs[q])]<-found.uptimes[q]
        }
        colnames(grape.df)<- c(start.col.names, as.character(date))
    }
    return(grape.df)
}



#

