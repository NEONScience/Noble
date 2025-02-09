#' @title Date Extract
#' @author Robert Lee \email{rlee@battelleecology.org}\cr
#'
#' @description Extract data between specified start and end times.
#'
#' @param \code{data} A data frame generated by the Noble package.
#' @param \code{bgn.date} The beginning time to extract data for, as YYYY-MM-DD hh:mm:ss
#' @param \code{end.date} The end time to extract data for, as YYYY-MM-DD hh:mm:ss
#'
#' @return A data frame with data collected between \code{bgn.time} and \code{end.time}.
#'
#' @keywords data, subset, date,
#'
#' @examples
#' \dontrun{
#' data=Noble::data.pull(site = "CPER", dpID = "DP1.00001.001", bgn.month = "2017-08",
#' end.month = "2017-08", time.agr = 30, save.dir = tempdir())
#' ## Extract data from Aug 8th, UTC.
#' aug8=ml.extract(data=data, bgn.date = "2017-08-08", end.date = "2017-08-09")
#' }
#' @seealso \code{ml.extract}
#'
# # changelog and author contributions / copyrights
#   Robert Lee (2017-10-26)
#     original creation
#
##############################################################################################

## Funciton start
date.extract=function(data, bgn.date, end.date){
    data.out=data[which(as.POSIXct(data[,1], tz = "UTC")>bgn.date & as.POSIXct(data[,1], tz="UTC")<end.date),]
    return(data.out)
}
