############################################################################################
#' @title  Downloads data for a specified data product or products, and saves the data to a specified directory

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description For the specified dates, site, package parameters, and data product or name of family of data products,
#' data are downloaded and saved to the specifed directory.
#'
#' @param \code{site} Parameter of class character. The NEON site data should be downloaded for.
#' @param \code{dpID} Parameter of class character. The name of the data product to pull data, or a keyword for a family of data products, e.g. "wind" will pull for 2D and 3D wind data products.
#' @param \code{bgn.month} Parameter of class character. The year-month (e.g. "2017-01") of the first month to get data for.
#' @param \code{end.month} Parameter of class character. The year-month (e.g. "2017-01") of the last month to get data for.
#' @param \code{time.agr} Parameter of class numeric. The data agregation interval requested, must be 1, 2, or 30.
#' @param \code{package} Parameter of class character. Optional. The type of data package to be returned If not specified, defaults to basic.
#' @param \code{save.dir} Parameter of class character. The local directory where data files should be saved.
#'
#' @return Writes data files to the specified directory.

#' @keywords process quality, data quality, gaps, commissioning

#' @examples
#' #Make a temporary direcotry for the example:
#' tempDir<- tempdir()
#' data.pull(site = "CPER", dpID = "Radiation", bgn.month = "2017-02", end.month = "2017-03", time.agr = 30, package="expanded", save.dir = tempDir)
#' data.pull(site = "CPER", dpID = "DP1.00002.001", bgn.month = "2017-04", end.month = "2017-05", time.agr = 30, package="basic", save.dir= tempDir)


#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Robert Lee (2017-07-18)
#     original creation
#
##############################################################################################


###TEST BLOCK####
# site="CPER"
# dpID="DP1.00002.001"
# bgn.month="2017-06"
# end.month="2017-06"
# time.agr=30
# package="basic"


data.pull = function(site = "JORN", dpID = "DP1.00001.001", bgn.month = "2017-02", end.month = "2017-04", time.agr = 30, package="basic", save.dir){

    require(jsonlite)
    #require(nneo)
    require(lubridate)

    #read in current TIS site info
    is_site_config<-Noble::is_site_config
    curr_site_config=is_site_config[which(is_site_config$SiteID==site),]

    #make sure to request valid packages!
    valid.pack<-c("basic", "expanded")
    save.dir = paste0(save.dir, "/")

    #if(!dir.exists(save.dir)){stop("Invalid directory specified! Please correct the parameter given to 'save.dir'.")}

    if(missing(package)){package<-"basic"}
    if(!package %in% valid.pack){stop("Please specify a package of 'basic' or 'expaned'")}

    #figure out if a code or keyword for a data product has been passed to the fucntion.
    if(!grepl(pattern = "^DP1.*", x=dpID)){

        stop("Please enter a data product code, eg: dpID='DP1.00001.001'.")
    }

    # Make a sequence of dates and times for the requested period
    bgn_temp <- as.Date(paste0(bgn.month, "-01"), tz="UTC")
    end_temp <- as.Date(paste0(end.month, "-01"), tz="UTC")
    bgn_temp <- as.POSIXct(paste0(bgn.month, "-01"), tz="UTC")
    end_temp<- as.POSIXlt(paste0(end_temp, "-01"), tz="UTC")
    end_temp$mon<-end_temp$mon+1
    end_temp<-end_temp-lubridate::minutes(time.agr)-lubridate::seconds(1)

    ref_seq<-Noble::help.time.seq(from=bgn_temp, to=end_temp, time.agr = time.agr)

    # Get site metadata
    call.df=Noble:::.gen.call.df(bgn.month=bgn.month,
                                 end.month=end.month,
                                 site=site, dpID=dpID,
                                 time.agr=time.agr,
                                 package=package)

    #Make our start timestamps, which data are matched to.
    start_time_stamps<-as.data.frame(ref_seq)

    ##### Data Pull section #####
    #Set the expected data filename for the data product
    file.name<- paste0("NEON.", curr_site_config$Domain,".", site, ".", dpID, "_REQ_", bgn_temp, "_", as.character(as.Date(end_temp)), "_", time.agr, "min_", package,".csv.gz")

    #if we're missing the expected file, download the data
    if(!file.exists(paste0(save.dir, file.name))){

        #how many locations exist for the product, according to the API? Make a list of those
        loc_per_dp<-unique(call.df$loc_list[call.df$dp_list==dpID])

        # if we're going around the loop again, get rid of the previous DP's data
        if(exists("full.df")){
            rm(full.df)
        }

        # Loop for gettng one location's DP data- (eg. ML1's data)
        for(j in 1:length(loc_per_dp)){

            #set location to first in list
            temp_loc<-loc_per_dp[j]

            #pull out all URLs for the DP we need
            temp_urls_per_dp<-call.df$url_list[which(call.df$loc_list==temp_loc & call.df$dp_list==dpID)]

            if(exists("temp.dp.data")){
                rm(temp.dp.data)
            }

            for(k in 1:length(temp_urls_per_dp)){

                #clear out our temp dp data object, before adding to it.
                if(!exists("temp.dp.data")){
                    temp.dp.data<-read.csv(file=as.character(temp_urls_per_dp[k]), stringsAsFactors = F)

                    #
                }else{
                    #Read in a month's worth of data
                    temp.data<-read.csv(file=as.character(temp_urls_per_dp[k]), stringsAsFactors = F)
                    temp.dp.data<-try(rbind(temp.dp.data, temp.data))
                }

            }
            # Convert returned data to POSIX
            temp.dp.data$startDateTime<-as.POSIXct(temp.dp.data$startDateTime, format= "%Y-%m-%dT%H:%M:%SZ", tz="UTC")

            # Add loacation data to the column names, then clean up the date/time names
            colnames(temp.dp.data)<-paste0(colnames(temp.dp.data),".", temp_loc)
            colnames(temp.dp.data)[grepl(pattern = "*Time*", x=colnames(temp.dp.data))]=
                gsub(pattern = "\\d||\\.", replacement = "",
                     x = colnames(temp.dp.data[,grepl(pattern = "*Time*", x=colnames(temp.dp.data))]))


            if(!exists("full.df")){
                full.df<-data.frame(matrix(NA, nrow = length(ref_seq), ncol = length(colnames(temp.dp.data))))
                names(full.df)<-colnames(temp.dp.data)
                full.df[,1]<- as.POSIXct(ref_seq)


                #print(length(full.df[,1]))

                # New fix?
                full.df[which(full.df[,1] %in% temp.dp.data$startDateTime),]<-temp.dp.data[which(temp.dp.data$startDateTime %in% full.df[,1]),]

                # The old way of adding to frame, might be broken
                #full.df[which(full.df[,1] %in% temp.dp.data$startDateTime),]<-temp.dp.data[which(full.df[,1] %in% temp.dp.data$startDateTime),]


                #print(length(full.df[,1]))
            }else{
                temp.full.df<-data.frame(matrix(NA, nrow = length(ref_seq), ncol = length(colnames(temp.dp.data))))
                names(temp.full.df)<-colnames(temp.dp.data)
                temp.full.df[,1]<-as.POSIXct(ref_seq)

                #New way?
                #temp.full.df[which(full.df[,1] %in% temp.dp.data$startDateTime),]<-temp.dp.data[which(temp.dp.data$startDateTime %in% full.df[,1]),]

                # The old way of adding to frame, might be broken
                temp.full.df[which(full.df[,1] %in% temp.dp.data$startDateTime),]<-temp.dp.data[which(full.df[,1] %in% temp.dp.data$startDateTime),]

                rm<-c(1,2)
                full.df<-cbind(full.df, temp.full.df[,-rm])
                #print(length(full.df[,1]))
            }
        }
        #remove randow all na rows (artefacts, not gaps)
        #full.df<-full.df[-which(is.na(full.df[,1])),]

        #Make sure timestamps come out in order
        #full.df<-full.df[order(full.df[,1]),]

        file.path<-paste0(save.dir, file.name)
        zip.dir<-gzfile(file.path)
        write.csv(x=full.df, file=zip.dir, row.names = F)
        return(full.df)

    }else{
        full.df<-read.csv(paste0(save.dir, file.name))
        return(full.df)
    }
    rm(full.df)

}
