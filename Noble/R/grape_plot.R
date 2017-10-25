.grape.plot<-function(bgn.date, end.date, system, site, save.dir){

    list.test.dirs=Noble:::.grape.scrape(bgn.date=bgn.date, end.date=end.date, system = system, test.sites = site, save.dir = save.dir)

    Noble:::.grape.list(list.test.dirs=list.test.dirs)
    out.df<-Noble:::.grape.df(site, log.dir = save.dir, system = system)

    m.ex.df<-data.table::melt(out.df, id.vars="Grape.MAC.Address")
    m.ex.df$variable<-as.Date(m.ex.df$variable)

    ggplot2::ggplot(data = m.ex.df, aes(x=variable , y=value))+geom_line()+
        facet_wrap(facets = ~Grape.MAC.Address)+
        scale_x_date(date_breaks = "2 weeks")+
        scale_y_discrete(limits=c(0,1))+
        labs(title=paste0(site, " Grape uptimes"), x="Date", y="Uptime (decimal percent)")+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    save.dir = paste0(save.dir, "/", site, "_Grapes.pdf")
    ggplot2::ggsave(filename = save.dir,width = 11, height = 8.5, units = "in")
}
