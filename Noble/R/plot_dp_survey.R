############################################################################################
#' @title  Save Summary Graphs of Data Product Health by Month

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description For a specified data product ID, this function will produce summary plots of data
#' product availability and validity for their period of record at a site. By default, plots for all
#' sites with data product availability are generated and saved to the specified directory. If
#' \code{site} is specified, only plots for the site(s) passed to that parameter are generated and saved.
#'
#'
#'
#' Because the full period of record for all sites are queried,
#' this function can take a long time to execute.
#'
#'
#' @param \code{dpID} Parameter of class character. The NEON data product code of the data product of interest.
#' @param \code{save.dir} The directory for data files and output PNGs to be saved to.
#' @param \code{site} Optional. Can specify single site or list of sites of interest.

#' @return Outputs a a PDF of plots data on of all measurement levesl, with one PDF per site.
#' If only one site is specified, the GGPlot2 object for the summary plot is also returned,
#' for use in automated report writing.

#' @keywords process quality, data quality, gaps, commissioning, data product, health

#' @examples
#' # For 2d Wind, save all plots to the current working directory:
#' dp.survey(dpID = "DP1.00001.001", save.dir = getwd())

#' @seealso Currently none
#'
#' @export

# changelog and author contributions / copyrights
#   Robert Lee (2016-07-017)
#     original creation
#
#   Robert Lee (2017-11-21)
#     Working version
#
#   Robert Lee (2017-11-26)
#     Color Revamp
#
##############################################################################################
plot.dp.survey=function(dpID, save.dir, site, pri.var){
    require(zoo)
    if(missing(pri.var)){
        pri.var=Noble::tis_pri_vars$data.field[Noble::tis_pri_vars$dpID==dpID]
    }
    var.name=gsub(pattern = "mean", replacement = "", x = pri.var, ignore.case = T)

    dp.avail = Noble::NEON.avail(dpID = dpID)
    dp.avail = cbind(Month=dp.avail[,1],  dp.avail[,which(colnames(dp.avail) %in% Noble::tis_site_config$SiteID)])

    if(missing(site)){
        dp.sites = colnames(dp.avail[,2:length(colnames(dp.avail))])
        #dp.sites = dp.sites[which(dp.sites %in% Noble::tis_site_config$SiteID)]
    }else{
        dp.sites=site
    }

    for(i in 1:length(dp.sites)){
        health.data=Noble::health.data(site = dp.sites[i], dpID = dpID)
        health.data$Month=as.Date(paste0(health.data$Month, "-01"))

        melt.health=reshape2::melt(data=health.data, id.vars="Month")

        plot=ggplot2::ggplot(data=melt.health, aes(x = Month, y = value))+
            ggplot2::geom_col(aes(fill=variable), position = position_identity())+
            ggplot2::scale_fill_manual(values = c("#062372", "#5483c4"), name="Metric")+ # Availability then Validity
            ggplot2::theme_bw()+
            ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")+
            ggplot2::scale_y_continuous(limits = c(0,100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))+
            ggplot2::ylab("Percent")+
            ggplot2::ggtitle(label = dp.sites[i], subtitle = paste0("Using '", pri.var, "' as data column"))+
            ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            ggplot2::geom_hline(yintercept = 90, color="#a02c46", show.legend = T)


        graphics.off()

        ggsave(filename = paste0(dpID, "_health_", dp.sites[i], ".png"), plot = plot, device = "png", path = save.dir, width = 7, height = 5.5, units = "in")
    }
    #if(length(dp.sites==1)){return(plot)}
}

