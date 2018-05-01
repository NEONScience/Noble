############################################################################################
#' @title  Downloads data for a specified data product or products, and saves the data to a specified directory

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description For the specified dates, site, package parameters, and data product or name of family of data products,
#' data are downloaded and saved to the specifed directory.
#'
#' @param \code{site} Parameter of class character. The NEON site data should be downloaded for.
#' @param \code{dpID} Parameter of class character. The data product code in question. See
#' \code{Noble::tis_pri_vars} for a selected list of data product names and codes, or
#' \url{http://data.neonscience.org/data-product-catalog} for a complete list.
#' @param \code{bgn.month} Parameter of class character. The year-month (e.g. "2017-01") of the first month to get data for.
#' @param \code{end.month} Parameter of class character. The year-month (e.g. "2017-01") of the last month to get data for.
#' @param \code{time.agr} Parameter of class numeric. The data agregation interval requested, must be 1, 2, or 30.
#' @param \code{package} Parameter of class character. Optional. The type of data package to be returned If not specified, defaults to basic.
#' @param \code{save.dir} Parameter of class character. The local directory where data files should be saved.
#' @param \code{complete.times} Optional. Parameter of class logical. Should gaps in the endDateTime column be filled? Defaults to FALSE (No gap filling).
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
#
##############################################################################################


# ###TEST BLOCK####
# site="MLBS"
# dpID="DP1.00040.001"
# bgn.month="2018-02"
# end.month="2018-03"
# time.agr=30
# package="basic"


data.pull = function(site, dpID, bgn.month, end.month, time.agr, package="basic", save.dir, complete.times=F){
    bgn_temp <- as.Date(paste0(bgn.month, "-01"), tz="UTC")
    end_temp <- as.Date(paste0(end.month, "-01"), tz="UTC")

    #Make a list of months to get data for
    date_range<-substr(seq.Date(bgn_temp, end_temp, "month"), 0, 7)

    #read in current IS site info
    is_site_config<-Noble::is_site_config
    curr_site_config=is_site_config[which(is_site_config$SiteID==site),]

    #make sure to request valid packages!
    valid.pack<-c("basic", "expanded")
    #save.dir = paste0(save.dir, "/")

    if(!dir.exists(save.dir)){stop("Invalid directory specified! Please correct the parameter given to 'save.dir'.")}

    if(missing(package)){package<-"basic"}
    if(!package %in% valid.pack){stop("Please specify a package of 'basic' or 'expaned'")}

    #figure out if a code or keyword for a data product has been passed to the fucntion.
    if(!grepl(pattern = "^DP1.*", x=dpID)){

        stop("Please enter a data product code, eg: dpID='DP1.00001.001'.")
    }

    # Make a sequence of dates and times for the requested period
    bgn_temp <- as.POSIXct(paste0(bgn.month, "-01"), tz="UTC")
    end_temp<- as.POSIXlt(paste0(end_temp, "-01"), tz="UTC")
    end_temp$mon<-end_temp$mon+1
    end_temp<-end_temp-lubridate::minutes(time.agr)-lubridate::seconds(1)

    # make a reference sequence
    ref_seq<-Noble::help.time.seq(from=bgn_temp, to=end_temp+lubridate::seconds(1), time.agr = time.agr)

    # Get site metadata
    call.df=as.data.frame(Noble:::.gen.call.df(bgn.month=bgn.month,
                                 end.month=end.month,
                                 site=site, dpID=dpID,
                                 time.agr=time.agr,
                                 package=package))

    #Make our start timestamps, which data are matched to.
    start_time_stamps<-as.data.frame(ref_seq)

    ##### Data Pull section #####
    #Set the expected data filename for the data product
    file.name<- paste0("NEON.", curr_site_config$Domain,".", site, ".", dpID, "_REQ_", bgn_temp, "_", as.character(as.Date(end_temp)), "_", time.agr, "min_", package,".csv.gz")

    ## If the file isn't there, get it
    if(!file.exists(paste0(save.dir, file.name))){
        data.wad=lapply(date_range, function(m) lapply(call.df$url_list[grepl(x=call.df$url_list, pattern = m)],
                                                       function(l) as.data.frame(read.csv(as.character(l)), stringsAsFactors = F))) #Get all data in one lump, (list of lists of data frames)
        data.lump=do.call(rbind, data.wad) #make into data frame of lists, with dimensions nrow=n_months, ncol=n_measurementLocations
        data.chunk=lapply(seq(length(data.lump[1,])), function(x) do.call(rbind, data.lump[,x])) # merge down rows, so that only data frames of measurement levels exist

        ## Clean up column naming (apply location info to measurement columns)
        for(i in 1:length(data.chunk)){
            colnames(data.chunk[[i]])[which(!grepl(x = colnames(data.chunk[[i]]), pattern = "time", ignore.case = T))]=
                paste0(colnames(data.chunk[[i]][which(!grepl(x = names(data.chunk[[i]]), pattern = "time", ignore.case = T))]), ".", unique(call.df$loc_list)[i])
            data.chunk[[i]]$startDateTime=as.POSIXct(data.chunk[[i]]$startDateTime, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")
        }

        # Make a reference sequence to match to
        dates=data.frame(startDateTime=ref_seq)

        #Perform the matching
        data.raw=data.frame(lapply(data.chunk, function(x) dplyr::left_join(x=dates, y=x, by="startDateTime")))

        #strip out unneeded datetime columns
        time.index=grep(x = colnames(data.raw), pattern = "time", ignore.case = T)
        if(length(time.index)>2){
            data.out=data.raw[,-(time.index[3:length(time.index)])]
        }else{
            data.out=data.raw
        }

        # ## Remove duplicates
        if(length(which(duplicated(data.out)))>0){
            data.out=data.out[-which(duplicated(data.out)),]
        }

        ## Fill a sequence of endDateTimes?
        if(complete.times){
            end_ref=as.POSIXct(ref_seq, tz = "UTC")+lubridate::minutes(x=time.agr)
            data.out$endDateTime=end_ref
        }

        #Zip and write the files
        file.path<-paste0(save.dir, file.name)
        zip.dir<-base::gzfile(file.path, open = "wb")
        write.csv(x=data.out, file=zip.dir, row.names = F)
        close(zip.dir)
    }else{#if the file is there, read it
        data.out<-read.csv(paste0(save.dir, file.name))
    }
    ## Return to parent environment
    return(data.out)
}

