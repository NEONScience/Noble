############################################################################################
#' @title  Returns a list of sites where a given data product exists in a given time range

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description For a given data product ID and the start and end months, a list of sites where the data product is has data is returned.
#'
#' @param \code{dpId} Parameter of class character. The data product ID of interest, must be in code format, eg. "DP1.00001.001"
#' @param \code{bgn.month} Parameter of class character. The start month of the sequence of interest.
#' @param \code{end.month} Parameter of class character. The (inclusive) end month of the sequence of interest.
#'
#' @return A list of sites where the data product is available between the two input months.
#'
#'
#' @keywords process quality, data quality, gaps, commissioning

#' @examples
#' \dontrun{
#' 2d_wind=test.sites(dpId="DP1.00001.001")
#' }
#'
#' @seealso \code{\link{NEON.avail}}, which returns a data frame of data product availability by site and month.

# changelog and author contributions / copyrights
#   Robert Lee (2017-07-18)
#     original creation
#
##############################################################################################

test.sites = function(dpID, bgn.month, end.month){
    prod.avail=NEON.avail(dpID = dpID)
    bgn.avail=zoo::as.yearmon(bgn.month)
    end.avail=zoo::as.yearmon(end.month)
    tis_site_config=Noble::tis_site_config
    test.avail=rbind(prod.avail[prod.avail$Month==bgn.avail,], prod.avail[prod.avail$Month==end.avail,])

    indx.test.sites=c()
    for(i in 2:length(colnames(test.avail))){
        if(all(complete.cases(test.avail[,i]))){
            temp=test.avail[,i]
            indx.test.sites=append(indx.test.sites, i)
        }
    }

    test.sites=colnames(test.avail[,indx.test.sites])
    test.sites=test.sites[test.sites %in% as.character(tis_site_config$SiteID)]
    return(test.sites)
}
