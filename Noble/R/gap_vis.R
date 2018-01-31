############################################################################################
#' @title  Generate Data Product Gap Visualization

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description For a specified data product ID, this function will produce a visual representation of
#' data and quality flag gaps for their period of record at a site.
#'
#' @param \code{site} A NEON TIS site
#' @param \code{dpID} Parameter of class character. The NEON data product code of the data product of interest.
#' @param \code{bgn.month} The fisrt month of data to examine for gaps.
#' @param \code{end.date} The last month of data to examine for gaps.
#' @param \code{save.dir} The directory to write data files to.


#' @return Writes a PNG showing the starts and ends of gaps, by ML and gap type

#' @keywords process quality, data quality, gaps, commissioning, data product, health

#' @examples
#' \dontrun{
#' # For 2d Wind, save files to the current working directory:
#' gap.vis(site="CPER", dpID = "DP1.00001.001", bgn.month="2017-07", end.month="2017-07", save.dir = getwd())
#' }

#' @seealso gap.find, gap.report
#'
############################################################################################

gap.vis=function(site, bgn.month, end.month, dpID, save.dir){
    require(ggplot2)

    data=Noble::data.pull(dpID = dpID, site = site, bgn.month = bgn.month, end.month = end.month, time.agr = 30, package = "basic", save.dir = tempdir())
    short.name=Noble::tis_pri_vars$short.name[Noble::tis_pri_vars$dpID==dpID]
    raw.mls=zoo::na.trim(stringr::str_extract(string = colnames(data), pattern = "\\.\\d\\d\\d\\.\\d\\d\\d"))
    tower.mls=as.numeric(unique(stringr::str_sub(raw.mls, start = 7, end = 7)))

    ## Below function from answer provided by Joris Meys, on stackoverflow.com.
    ## Retrieved Dec 20, 2017 from https://stackoverflow.com/questions/7077710/sequence-length-encoding-using-r?rq=1
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

    no.data.times=data.frame(no.data.times, gap.type=rep("No Data", times=length(no.data.times[,1])))
    no.qf.times=data.frame(no.qf.times, gap.type=rep("No QFs", times=length(no.qf.times[,1])))
    gaps=rbind(no.data.times, no.qf.times)


    gaps$ML=paste0("ML-", gaps$ML)


    gap.vis=ggplot2::ggplot(gaps,
                            ggplot2::aes(x=factor(ML),
                                         ymin=as.POSIXct(gap.start),
                                         ymax=as.POSIXct(gap.end),
                                         color=factor(ML)
                            )
    )+
        ggplot2::geom_linerange(size=2.5)+
        ggplot2::facet_grid(gap.type+ML~., scales="free_x", switch = "y")+
        ggplot2::coord_flip()+
        ggplot2::scale_x_discrete(name="", breaks=NULL)+
        ggplot2::theme_minimal()+
        ggplot2::guides(fill=FALSE)+
        ggplot2::ggtitle(label = site, subtitle = paste0(dpID, " Gaps"))+
        ggplot2::theme(legend.position="none")

    ggplot2::ggsave(filename = paste0(short.name, "_", site, "_Gaps","_", bgn.month, "-", end.month, ".png"),
           plot = gap.vis,
           device = "png",
           path = save.dir,
           width = 8,
           height = 5,
           units = "in")
}
