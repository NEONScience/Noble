## Tests block

##
.grape.num.plot <- function(site, system, log.dir, save.dir){
    if(missing(log.dir)){message("Reading logs from SCA")
        grape.df=Noble:::.grape.df(site = site, system = system)
        }else{grape.df=Noble:::.grape.df(site = site, log.dir = log.dir, system = system)
        }

    grape.df=Noble:::.grape.df(site = site, system = system)
    num.grapes = (colSums(!is.na(grape.df[,2:length(colnames(grape.df))])))
    melt.num.grapes = data.table::melt(num.grapes)

    melt.num.grapes = data.frame(date=rownames(melt.num.grapes), grape.count=melt.num.grapes$value)
    melt.num.grapes$date<-as.Date(melt.num.grapes$date)
    plot<-ggplot2::ggplot(data=melt.num.grapes, aes(x=date, y=num.grapes))+
        ggplot2::geom_line(colour="#5a58e2")+
        ggplot2::theme_bw()+
        ggplot2::labs(x="Date", y="Number of Grapes", title=site)+
        ggplot2::scale_y_continuous(limits = c(0, max(melt.num.grapes$grape.count)))


    if(missing(save.dir)){
    }else{message(paste0("trying to save to ", save.dir))
        ggplot2::ggsave(filename = paste0(site, "_grape_counts.png"), plot = plot, device = "png", path = save.dir, width = 6, height = 3.75, units = "in", dpi = 300)
    }
    base::return(plot)
}
