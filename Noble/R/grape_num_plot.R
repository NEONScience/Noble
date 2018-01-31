## Tests block

##
.grape.num.plot <- function(site, system, log.dir, save.dir){
    library(ggplot2)
      Mode <- function(x) {
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
    }

     if(missing(log.dir)){message("Reading logs from SCA")
        grape.df=Noble:::.grape.df(site = site, system = system)
        }else{grape.df=Noble:::.grape.df(site = site, log.dir = log.dir, system = system)
        }

   # grape.df=Noble:::.grape.df(site = site, system = system)

    #make sequence of dates of interest
    date.seq=seq.Date(from=as.Date(colnames(grape.df)[2]), to=as.Date(colnames(grape.df)[length(colnames(grape.df))]), by=1)

    #calcualte number fo grapes per day
    num.grapes = (colSums(!is.na(grape.df[,2:length(colnames(grape.df))])))

    #All this is to make NAs in a continuous sequence, so plot has gaps to indicate missing data
    date.seq=seq.Date(from=as.Date(colnames(grape.df)[2]), to=as.Date(colnames(grape.df)[length(colnames(grape.df))]), by=1)
    na.list=rep(NA, times=length(date.seq))
    names(na.list)=date.seq
    na.list[which(names(na.list)%in%names(num.grapes))]=num.grapes[which(names(na.list)%in%names(num.grapes))]
    num.grapes=na.list


    melt.num.grapes = data.table::melt(num.grapes)

    melt.num.grapes = data.frame(date=rownames(melt.num.grapes), grape.count=melt.num.grapes$value)
    melt.num.grapes$date<-as.Date(melt.num.grapes$date)

    #generate some stats
    mode=Mode(melt.num.grapes$grape.count)
    max=max(melt.num.grapes$grape.count, na.rm=T)


    plot<-ggplot2::ggplot(data=melt.num.grapes, ggplot2::aes(x=date, y=num.grapes))+
        ggplot2::geom_path(colour="#2c47cc", size=1)+
        #ggplot2::geom_point(colour="#e22522")+
        ggplot2::theme_bw()+
        ggplot2::labs(x="Date", y="Number of Grapes")+
        ggplot2::ggtitle(label=paste0(site, " Grapes"), subtitle = paste0("Mode: ", mode, ", Max: ", max) )+
        ggplot2::geom_hline(yintercept = mode, color="red")+
        ggplot2::scale_y_continuous(limits = round(c(0, max(melt.num.grapes$grape.count, na.rm=T)+1), digits = 0))


    if(missing(save.dir)){
    }else{message(paste0("Trying to save to ", save.dir))
        ggplot2::ggsave(filename = paste0(site, "_grape_counts.png"), plot = plot, device = "png", path = save.dir, width = 6, height = 3.75, units = "in", dpi = 300)
        message("File saved.")
    }
    base::return(plot)
}
