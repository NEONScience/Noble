############################################################################################
#' @title  Generate and Write Data Product Gap Summaries

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description For a specified data product ID, this function will produce summary CSVs of data
#' and quality flag gaps for their period of record at a site.
#'
#' @param \code{site} A NEON TIS site
#' @param \code{dpID} Parameter of class character. The NEON data product code of the data product of interest.
#' @param \code{bgn.month} The fisrt month of data to examine for gaps.
#' @param \code{end.date} The last month of data to examine for gaps.
#' @param \code{save.dir} The directory to write data files to.


#' @return Writes two CSVs of the start end times of gaps, one CSV for data gaps, the other for quality flag gaps.

#' @keywords process quality, data quality, gaps, commissioning, data product, health

#' @examples
#' # For 2d Wind, save files to the current working directory:
#' gap.report(site="CPER", dpID = "DP1.00001.001", bgn.month="2017-07", end.month="2017-07", save.dir = getwd())

#' @seealso gap.find
#'
############################################################################################
gap.report=function(site, bgn.month, end.month, dpID, save.dir){

    data=Noble::data.pull(dpID = dpID, site = site, bgn.month = bgn.month, end.month = end.month, time.agr = 30, package = "basic", save.dir = tempdir())
    short.name=Noble::tis_pri_vars$short.name[Noble::tis_pri_vars$dpID==dpID]
    raw.mls=zoo::na.trim(stringr::str_extract(string = colnames(data), pattern = "\\.\\d\\d\\d\\.\\d\\d\\d"))
    tower.mls=as.numeric(unique(stringr::str_sub(raw.mls, start = 7, end = 7)))

    ## Below function from answer provided by Joris Meys, on stackoverflow.com. Retrieved Dec 20, 2017 from https://stackoverflow.com/questions/7077710/sequence-length-encoding-using-r?rq=1
    seq.length <- function(x){

        if(!is.numeric(x)) x <- as.numeric(x)
        n <- length(x)
        y <- x[-1L] != x[-n] + 1L
        i <- c(which(y|is.na(y)),n)

        data.frame(
            start_index = as.vector(x[head(c(0L,i)+1L,-1L)]),
            stop_index = as.vector(diff(c(0L,i)))

        )
    }

    ## End Function

    data.time.table=function(l){
        ml.data=Noble::ml.extract(data=data, ml = l)
        gaps=Noble::find.gap(data = ml.data, time.agr = 30, return = "index")
        no.data.times=seq.length(gaps$no.data.indx)
        no.data.times$stop_index=(no.data.times$start_index+no.data.times$stop_index-1)
        no.data.times=data.frame(ML=l,gap.start=ml.data$startDateTime[no.data.times$start_index], gap.end=ml.data$startDateTime[no.data.times$stop_index])
    }

    qf.time.table=function(l){
        ml.data=Noble::ml.extract(data=data, ml = l)
        gaps=Noble::find.gap(data = ml.data, time.agr = 30, return = "index")
        no.qf.times=seq.length(gaps$no.qf.indx)
        no.qf.times$stop_index=(no.qf.times$start_index+no.qf.times$stop_index-1)
        no.qf.times=data.frame(ML=l,gap.start=ml.data$startDateTime[no.qf.times$start_index], gap.end=ml.data$startDateTime[no.qf.times$stop_index])
    }

    no.data=lapply(tower.mls, data.time.table)
    no.qf=lapply(tower.mls, qf.time.table)

    no.data.times=do.call(rbind, no.data)
    no.qf.times=do.call(rbind, no.qf)

    write.csv(x = no.data.times, file = paste0(save.dir, "/", site, "_", bgn.month, "-", end.month, "_", short.name, "_NO_DATA.csv"),row.names = F)
    write.csv(x = no.qf.times, file = paste0(save.dir, "/", site, "_", bgn.month, "-", end.month,  "_", short.name, "_NO_QF.csv"))

}
