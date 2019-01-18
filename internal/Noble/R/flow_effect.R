#saves a plot of flow metrics, and returns the percent of records with good flow (percent of flow metrics where flowPassQM=100)

.flow.effect=function(site, bgn.month, end.month, save.dir)
{
    domn=Noble::tis_site_config$Domain[Noble::tis_site_config$SiteID==site]
    data.dir=Noble:::.data.route(site=site, save.dir = save.dir)
    data=Noble::data.pull(site=site, dpID = "DP1.00002.001", bgn.month = bgn.month, end.month = end.month, package = 'expanded', time.agr = 30, save.dir = data.dir)

    flow.flags=data.frame(Date=data[,1], data[grepl(pattern = "flow", x = colnames(data), ignore.case = T)])
    #colnames(flow.flags)=gsub(pattern = "\\.||\\d", replacement = "", x = colnames(flow.flags))

    melt.flow=reshape2::melt(flow.flags, id.vars="Date", na.rm=T)
    melt.flow$variable=gsub(pattern = "\\.||\\d", replacement = "", x = melt.flow$variable)
    melt.flow$Date=as.POSIXct(melt.flow$Date)


    niceColors<- c("flowFailQM"="#f25841", "flowNAQM"="grey", "flowPassQM"="#41f299")

   # melt.flow=reshape2::melt(data=flags.only[grepl(pattern = "flow", x = colnames(flags.only), ignore.case = T)]

    #flags=ggplot(melt.flags, aes(x = Date, y = value, fill=factor(variable)))+geom_bar(stat = 'identity')

    plot=ggplot2::ggplot(melt.flow, ggplot2::aes(x=Date, y = value/3, fill=factor(variable)))+
        ggplot2::theme_bw()+
        ggplot2::geom_bar(stat = 'identity', width = 10000)+
        ggplot2::scale_y_continuous(limits = c(0,100))+
        ggplot2::theme(axis.text.x =element_blank())+
        ggplot2::scale_fill_manual(name = "Flow Quality Metrics", values=niceColors)+
        ggplot2::labs(x=paste0(zoo::as.yearmon(bgn.month, format="%Y-%m"), " to ", zoo::as.yearmon(end.month, format="%Y-%m")), y="Flagging distribution (% of measuremnts)", title=paste0(domn, "-", site))

    ggplot2::ggsave(filename = paste0(site, "_", bgn.month, "-", end.month, "_flow.png"), path = paste0(save.dir, "/"), plot = plot, device = 'png', width = 5, height = 3.5, units = "in", dpi=300)

    #math for percent of file that passes.
    pass=flow.flags[,grepl(x=colnames(flow.flags), pattern = "Pass")]
    sum.pass=sum(pass, na.rm=T)
    total=(length(flow.flags)/(Noble::tis_site_config$Num.of.MLs[Noble::tis_site_config$SiteID==site]-1))*length(flow.flags[,1])
    pcnt.flow.pass=round(sum.pass/total, digits = 2)
    return(pcnt.flow.pass)
    #
    # flow.pass=flow.flags[,grepl(colnames(flow.flags), pattern = "Pass")]
    # flow.fail=flow.flags[,grepl(colnames(flow.flags), pattern = "Fail")]
    # flow.na=flow.flags[,grepl(colnames(flow.flags), pattern = "NA")]
    # ### RETURNS rough percentage of time when flow over threshold
    # onlyFlow=round(sum(colSums(flow.pass, na.rm = T))/total)
    #
    #
    # melt.flow=data.table::melt(flow.flags)
    # ggplot(data = melt.flow, aes(x=variable, y=factor(value), color=factor(value)))+geom_col()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

}

