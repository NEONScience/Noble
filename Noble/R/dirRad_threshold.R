############################################################################################
#' @title  Calculates a thresholds for direct radiation

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description Given a data frame of NEON data, \code{find.gap} will return either a list rows or
#' list of timestamps. Each returned list is list of missing timestamps, missing data (all NAs),
#' and data with no values, but that are still quality flagged.\cr
#'
#' Whether rows of missing data or timestamps are returned depends on the return parameter.
#' If return = "index", the returned list components will be as follows:\cr
#' \cr
#'     miss.indx - The row numbers where timestamps and data were missing\cr
#'     no.data.indx - The row numbers where data are missing, but are quality flagged\cr
#'     no.qf.indx - The row numbers where all data and QFs were 'NA'\cr
#'
#' If return = "times", the returned list components will be as follows:\cr
#' \cr
#'     miss.times - The times where timestamps and data were missing\cr
#'     na.data.times - The times where all data and QFs were 'NA'\cr
#'     no.qf.times - The times where data are missing, but are quality flagged\cr
#'
#' @param \code{data} A data frame of NEON data returned from the API or SOM tool.
#' @param \code{time.agr} Optional, but recommended. The data aggregation period of the input data
#' (difference between timestamps in minutes), if not specified it will guess at the value
#' @param \code{return} Optional. Used to specifiy whether row numbers ("index") or timestamps
#' ("times") are returned. Defaults to row numbers if not specified.

#' @return Outputs a list of lists of row numbers or timestamps where data is missing.
#'

#' @keywords process quality, data quality, gaps, commissioning

#' @examples
#' Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Robert Lee (2017-07-10)
#     original creation
#
##############################################################################################

dirRad.threshold = function(site, bgn.month, end.month, excuse){
    require(RAtmosphere)

    lat=Noble::tis_site_config$Latitude[which(Noble::tis_site_config$SiteID==site)]
    lon=Noble::tis_site_config$Longitude[which(Noble::tis_site_config$SiteID==site)]
    bgn.date = as.Date(paste0(bgn.month, "-01"), tz="UTC")
    end.date = Noble::end.day.time(end.month = end.month, time.agr = 1440)
    end.date
    dates = Noble::help.time.seq(from = bgn.date, to = end.date, time.agr = 1440)
    dates=as.POSIXlt(dates)

    julian.dates=dates$yday

    sun.times=lapply(julian.dates, function(x) RAtmosphere::suncalc(d = x, Lat = lat, Long = lon, UTC = TRUE))
    sun.times=data.frame(cbind(julian.dates, do.call(rbind, sun.times)))
    sink=lapply(c(1, 2, 3), function(x) class(sun.times[,x])<-"numeric")
    rm(sink)
    mean.sun.up=mean(unlist(sun.times$sunrise))
    mean.sun.down=mean(unlist(sun.times$sunset))

    threshold=round(((mean.sun.down-mean.sun.up)/24)*100-excuse, digits=2)
    return(threshold)
}

