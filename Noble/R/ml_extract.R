############################################################################################
#' @title  Refine a data frame with multiple measurement levels down to the specified level

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description For the specified dates, site, package parameters, and data product or name of family of data products,
#' data are downloaded and saved to the specifed directory.
#'
#' @param \code{data} A data frame of instrumented data pulled with \code{data.pull}.
#' @param \code{ml} Desired measurement level, given as an integer (eg ml=1, corresponding to the first level at a tower)
#'
#' @return Returns a data frame of data just from the specified measurement level

#' @keywords process quality, data quality, gaps, commissioning

#' @examples Currently none

#' @seealso Currently none

# changelog and author contributions / copyrights
#   Robert Lee (2017-07-18)
#     original creation
#
##############################################################################################

ml.extract<-function(data, ml){
    tis_site_config<-Noble::tis_site_config
    time.indx <- grep(x=colnames(data), pattern = "time", ignore.case = T)
    ml.pattern <- paste0(".0", ml, "0")
    ml.data<-data[,which(grepl(colnames(data), pattern = ml))]
    ml.data<-cbind(data[,time.indx], ml.data)
    return(ml.data)
}
