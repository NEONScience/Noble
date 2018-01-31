#' @title Find USCRN Sites Closest to the Specified Site
#' @author Robert Lee \email{rlee@battelleecology.org}\cr
#'
#' @description Get the WBAN for the USCRN site closest to the specified NEON site, as well as the
#' approximate distance from the NEON site.
#'
#' @param \code{site} The NEON IS site of interest.
#'
#' @return A list with two elememnts: the WBAN for the USCRN site found, and the approximate distance
#' from the specified NEON site.
#'
#' @keywords USCRN, colocate, radius, site, WBAN
#'
#' @examples
#' \dontrun{
#' cper_colocate=uscrn.colocate("CPER")
#' }
#' @seealso
#'
# # changelog and author contributions / copyrights
#   Robert Lee (2018-01-10)
#     original creation
#
##############################################################################################

uscrn.colocate=function(site){
    uscrn=c()
    library(metScanR)
    while(length(uscrn)<1){
        out=metScanR::siteFinder(siteID = paste0("NEON:", site), network = "USCRN", radius = radius)
        uscrn=lapply(out, function(x) x$identifiers$id[x$identifiers$idType=="WBAN"])

        # if(length(uscrn)>1){
        #     print(paste0("Nearest USCRN site within a ", radius, " km radius of ", site, "."))
        # }
        radius=radius+5
    }
    uscrn.id=unlist(uscrn[1])
    out=c(uscrn.wban=uscrn.id, distance=(radius-5))
    return(out)
}
