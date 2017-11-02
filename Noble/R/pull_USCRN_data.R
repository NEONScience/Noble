############################################################################################
#' @title  Pull data from meterologic stations in the USCRN network

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description For the specified dates, site, package parameters, and data product or name of family of data products,
#' data are downloaded and saved to the specifed directory.
#'
#' @param \code{site} Parameter of class character. The NEON site data should be downloaded for.
#' @param \code{dp.name} Parameter of class character. The name of the data product to pull data, or a keyword for a family of data products, e.g. "wind" will pull for 2D and 3D wind data products.
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
#' data.pull(site = "CPER", dp.name = "Radiation", bgn.month = "2017-02", end.month = "2017-03", time.agr = 30, package="expanded", save.dir = tempDir)

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Robert Lee (2017-07-18)
#     original creation
#
##############################################################################################


# A function to grab data from the USCRN network.
#
# Provide a valid GHCND or WBAN ID for a site in the USCRN, the desired
# timescale of measurements, and requested begin and end times in POSIX format,
# and a data frame called USCRNData will be returned to the global environment.
# Another data frame called unitsByColumn is also returned to the golobal
# environment.
#
# Example inputs:
# timeScale <- "subhourly"
# stationID <- "USW00003047"
# TimeBgn <- as.POSIXct("2014-04-01 00:00:01", format="%Y-%m-%d %H:%M:%S", tz="UTC")
# TimeEnd <- as.POSIXct("2015-02-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz="UCT")
#
# grabUSCRN(timeScale = timeScale, TimeBgn = TimeBgn, TimeEnd = TimeEnd, stationID = stationID)
#
#------------------------------------------------------------------------------#

# library(readr)
# GHCNDstations <- data.frame(readr::read_delim("/Users/rlee/Dropbox/stations.csv", "\t", escape_double = FALSE, col_names = TRUE, trim_ws = TRUE))
#

pull.USCRN.data <- function(timeScale, stationID, TimeBgn, TimeEnd) {
  functionStart <<- Sys.time()
  options(stringsAsFactors=FALSE)
  library(data.table)
  library(stringr)

  ## Stop if the entered timeScale isn't valid ##
  if(!timeScale %in% c('monthly', 'daily', 'hourly', 'subhourly')){
    stop("Invalid 'timeScale'! Please enter one of the following: 'monthly', 'daily', 'hourly', 'subhourly'.")}

  ## Convert input times and grab years
  TimeBgn <- as.POSIXct(format(TimeBgn, tz="UTC", usetz= T), tz="UTC", usetz = T)
  TimeEnd <- as.POSIXct(format(TimeEnd, tz="UTC", usetz= T), tz="UTC", usetz = T)
  bgnYear <- substr(TimeBgn, 0, 4)
  endYear <- substr(TimeEnd, 0, 4)

  ## Get the column names for each data product ##
  ## Hourly data has to be different from the others, for some reason ##
  if(timeScale=="hourly"){
    header <- readLines(
      "ftp://ftp.ncdc.noaa.gov/pub/data/uscrn/products/hourly02/HEADERS.txt")
  }else{header <- readLines(paste0("ftp://ftp.ncdc.noaa.gov/pub/data/uscrn/products/", timeScale, "01/HEADERS.txt"))}
  colnames<-unlist(strsplit(header[2], " "))
  units <- unlist(strsplit(header[3], " "))
  unitsByColumn <- data.frame(rbind(units))
  unitsByColumn<<-`colnames<-`(unitsByColumn, colnames)

  ## Get and set some variables to build data download links ##
  uscrn <- readLines("https://www.ncdc.noaa.gov/crn/qcdatasets.html", warn = F)
  dataLines <- unlist(strsplit(uscrn[grep(pattern = paste0("/",timeScale), uscrn)], '"'))
  dataLinks <-unlist(dataLines[which(grepl("/data/", dataLines)&grepl("http", dataLines))])
  baseLink <- gsub("http", "https", dataLinks[which(min( nchar(dataLinks))==nchar(dataLinks))])

  #Convert the stationID input to WBAN, if possible
  wban<-substr(stationID, (nchar(stationID)-4), nchar(stationID))

  #Read station info for all sites in the USCRN


  station.Info <<- data.frame(read.table(
    "ftp://ftp.ncdc.noaa.gov/pub/data/uscrn/products/stations.tsv", header = T,
    sep = "\t"))
  station.Info<<-Noble::GHCNDstations
  monthlyFiles<-"https://www1.ncdc.noaa.gov/pub/data/uscrn/products/monthly01/"

  ## Find the USCRN location name for associated station ID ##
  location <- as.character(station.Info$LOCATION[grep(wban, station.Info$WBAN)])
  if (length(location)==0){ stop("Invalid station ID! Please enter a valid 5 digit WBAN ID for a station in the USCRN network.")}

  siteLat <- station.Info$LATITUDE[grep(wban, station.Info$WBAN)]
  siteLong <- station.Info$LONGITUDE[grep(wban, station.Info$WBAN)]

  siteTZInfo<-readLines(paste0("https://maps.googleapis.com/maps/api/timezone/json?location=", siteLat, ",", siteLong, "&timestamp=1458000000"))
  siteTZID <- unlist(strsplit(siteTZInfo[grep(siteTZInfo, pattern ="timeZoneId")], "\""))
  siteTZID <<- siteTZID[4]

  ##########################################
  ####  For dates within the same year  ####
  ##########################################

  if(bgnYear==endYear){
    ## Link to all data files available ##
    files<-paste0(baseLink, bgnYear, "/")
    if(timeScale=='monthly'){files<-monthlyFiles}

    ## Use location name to get requested dataset ##
    dataDir <- data.frame(strsplit(RCurl::getURL(files), split="\""))
    file <- grep(location, dataDir[,1])
    fileLoc<-paste0(files, dataDir[file[1],1])

    downloadStart<-Sys.time()
    print(paste("Beginning", stationID, "data download at", downloadStart))
    data1 <- data.frame(read.delim(fileLoc, header = F, sep = "",
                                   col.names = colnames))
    downloadEnd<-Sys.time()
    print(paste(stationID, "data download completed at", downloadEnd))
    print(paste("Elapsed time:", round(difftime(downloadEnd, downloadStart, units = "secs"), digits = 2), "seconds."))

    ## Turn dates and times into POSIX ##
    timeIdx <- grep("time", colnames(data1), ignore.case = T)
    dateIdx <- grep("date", colnames(data1), ignore.case = T)


    if(!timeScale %in% c("monthly","daily")){

      data1$UTC_TIME<-unlist(sapply(data1$UTC_TIME, function(x) str_pad(x, 4, pad = 0, side="left")))

      for(i in 1:length(dateIdx)){
        data1[,dateIdx[i]] <- paste(data1[,dateIdx[i]], data1[,timeIdx[i]])
      }

      data1<-data1[-timeIdx]
    }
    dateIdx <- grep("date", colnames(data1), ignore.case = T)

    if(timeScale=="daily"){
      data1[,dateIdx]<-as.POSIXct(as.character(data1[,dateIdx]), format = "%Y%m%d", tz="UTC")
      USCRNData<<-data1[which(between(data1$LST_DATE, TimeBgn, TimeEnd)),]
    }else if(timeScale=="monthly"){
      data1$LST_YRMO<-paste0(data1$LST_YRMO,"01")
      data1$LST_YRMO<-as.POSIXct(data1$LST_YRMO, format="%Y%m%d", tz="UTC")
      USCRNData<<-data1[which(between(data1$LST_YRMO, TimeBgn, TimeEnd)),]
    }else{
      data1$UTC_DATE<-as.POSIXct(as.character(data1$UTC_DATE), format = "%Y%m%d %H%M", tz="UTC")
      data1<-data1[-which(colnames(data1)=="LST_DATE")]
      USCRNData<<-data1[which(between(data1$UTC_DATE, TimeBgn, TimeEnd)),]
    }

    ###########################################
    #### For dates spanning multiple years ####
    ###########################################
  }else{
    yearSeq <- seq(from=as.numeric(bgnYear), to=as.numeric(endYear))
    data2<-data.frame()
    #Scenario 1: monthly data are requested - one download needed
    if(timeScale=="monthly"){
      dataDir <- data.frame(strsplit(RCurl::getURL(monthlyFiles), split="\""))
      file <- grep(location, dataDir[,1])
      fileLoc=paste0(monthlyFiles, dataDir[file[1],])
      downloadStart<-Sys.time()
      print(paste("Beginning", stationID, "data download at", downloadStart))
      data2 <- data.frame(read.delim(fileLoc, header = F, sep = "", col.names = colnames))
      downloadEnd<-Sys.time()
      print(paste(stationID, "data download completed at", downloadEnd))
      print(paste("Elapsed time:", round(difftime(downloadEnd, downloadStart, units = "secs"), digits = 2), "seconds."))


    #Scenario 2: Any other timescale is requested. there will be N number of dowloads (N is number of years spanned by request times)
      }else{
      downloadStart<-Sys.time()
      print(paste("Beginning", stationID, "data download at", downloadStart))

      # Loop over the requested years. Did you know only `lapply` is faster than a for loop in R?
      for(i in 1:length(yearSeq)){
          files<-paste0(baseLink, yearSeq[i], "/")
          dataDir <- data.frame(strsplit(RCurl::getURL(files), split="\""))
          file <- grep(location, dataDir[,1])
          fileLoc<-paste0(files, dataDir[file[1],1])
          data1 <- data.frame(read.delim(fileLoc, header = F, sep = "",
                                         col.names = colnames))
          data2<- rbind(data2, data1)
          }

      downloadEnd<-Sys.time()
      print(paste(stationID, "data download completed at", downloadEnd))
      print(paste("Elapsed time:", round(difftime(downloadEnd, downloadStart, units = "secs"), digits = 2), "seconds."))
    }

    #Setting colnames for downloaded data
    `colnames<-`(data2, colnames)

    if(!timeScale %in% c("monthly","daily")){
      ## Turn dates and times into POSIX ##
      timeIdx <- grep("time", colnames(data2), ignore.case = T)
      dateIdx <- grep("date", colnames(data2), ignore.case = T)

      data1$UTC_TIME<-unlist(sapply(data1$UTC_TIME, function(x) str_pad(x, 4, pad = 0, side="left")))

      for(i in 1:length(dateIdx)){
        data2[,dateIdx[i]] <- paste(data2[,dateIdx[i]], data2[,timeIdx[i]])
      }

      data2<-data2[-timeIdx]
    }

    dateIdx <- grep("date", colnames(data2), ignore.case = T)

    ## Truncate data to requested times ##
    if(timeScale=="daily"){
      data2[,dateIdx]<-as.POSIXct(as.character(data2[,dateIdx]), format = "%Y%m%d", tz="UTC")
      USCRNData<<-data2[which(between(data2$LST_DATE, TimeBgn, TimeEnd)),]
    }else if(timeScale=="monthly"){
      data2$LST_YRMO<-paste0(data2$LST_YRMO,"01")
      data2$LST_YRMO<-as.POSIXct(data2$LST_YRMO, format="%Y%m%d", tz="UTC")
      USCRNData<<-data2[which(between(data2$LST_YRMO, TimeBgn, TimeEnd)),]
    }else{
      data2$UTC_DATE<- as.POSIXct(data2$UTC_DATE, format = "%Y%m%d %H%M", tz="UTC")
      data2<-data2[-which(colnames=="LST_DATE")]
      USCRNData<<-data2[which(between(data2$UTC_DATE, TimeBgn, TimeEnd)),]
    }
    return(USCRNData)

  }
  # functionEnd <<- Sys.time()
  # print("Total function time:", round(difftime(functionEnd, functionStart, units = "secs"), digits = 2))
}
