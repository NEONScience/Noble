############################################################################################
# title  Create wind roses for NEON instrumented sites

# author Robert Lee \email{rlee@battelleecology.org}\cr

# description For a spceified site and time range, produce a wind rose plot. If the "ml" (measurement level)
# parameter is specified, a ggplot2 object for that measurement level is produced. Otherwise, a ggplot2 object of a
# faceted plot of all available measurement levels is returned.
#
# param \code{site} NEON site to produce the wind rose plot.
# param \code{bgn.month} The start month for wind data to plot.
# param \code{end.month} The end month for wind data to plot.
# param \code{min.speed} Optional. Used to specifiy a lower bound on wind speed. Defaults to 0.5 m/s.
# param \code{save.dir} The directory to save the summary PDF to.
#
# return Outputs a ggplot2 object of the generated wind roses

# keywords process quality, data quality, gaps, commissioning

# examples
# CPER<-plot.wind.rose(site="CPER", bgn.month="2017-01", end.month="2017-02", ml=2, speed.bins=10, dir.bins=36)

# seealso Currently none

# changelog and author contributions / copyrights
#   Robert Lee (2017-07-10)
#     original creation
#
##############################################################################################

wind.rose.report = function(sites, bgn.month, end.month, min.speed=0.5, save.dir, qf.plot="ALL"){
    if(!as.character(qf.plot) %in% c("0", "1", "ALL")){message("Plotting all QF types. Please specify 0 or 1 to subset on a QF value.")
        qf.plot="ALL"}
    library(magrittr)
    seasons=list("winter"=c(1:3),
                 "spring"=c(4:6),
                 "summer"=c(7:9),
                 "fall"=c(10:12))

    time.agr = 30

    # Set default bin breakdowns
    speed.bins=10
    dir.bins=36

    # Massage dates for checking
    bgn_temp <- as.Date(paste0(bgn.month, "-01"), tz="UTC")
    end_temp <- as.Date(paste0(end.month, "-01"), tz="UTC")
    end_temp<- as.POSIXlt(paste0(end_temp, "-01"), tz="UTC")
    end_temp$mon<-end_temp$mon+1
    end_temp<-end_temp-lubridate::minutes(time.agr)-lubridate::seconds(1)

    # Warn about being too ambitious with this
    #if(as.numeric(difftime(end_temp, bgn_temp))>=92){message("More than 3 months of data requested, may take a long time...")}

    pdf(file = paste0(save.dir, "/wind_directions.pdf"), onefile = T, paper = "US", width = 7.5, height = 10)

    sites=sites[order(unlist(lapply(sites, function(s) Noble::tis_site_config$Domain[Noble::tis_site_config$SiteID==s])))]

    for(s in 1:length(sites)){
        site=sites[s]
        domain=Noble::tis_site_config$Domain[Noble::tis_site_config$SiteID==site]
        # Return data
        data=try(Noble::data.pull(site = site, dpID = "DP1.00001.001", bgn.month = bgn.month, end.month = end.month, time.agr = 30, package="basic", save.dir = tempdir()))
        if(class(data)=="try-error"){
            plot=ggplot2::ggplot(data.frame()) + ggplot2::geom_point() + ggplot2::ggtitle(label = paste0(site, " wind data from ", bgn.month, " through ", end.month), subtitle = "NO DATA")
            print(plot)
        }else if(!class(data)=="try-error"){
            # Break out what MLs were returned
            temp<-(strsplit(colnames(data[,3:length(colnames(data))]), split = "\\."))
            temp=do.call(rbind, temp)
            #temp<-data.frame(temp[-1], stringsAsFactors=F)
            #temp<-unlist((temp[3,]))
            mls<-unique(temp[,3])
            clean.mls<-paste0("ML-", gsub(x=mls, pattern = "0", replacement = ""))

            # Build a DF of relevant data and MLS associated
            all<-NULL
            for(n in 1:length(mls)){
                data.by.ml<- data[,c(1,which(grepl(colnames(data), pattern=mls[n])))]

                dir.indx<- as.numeric(grep(x=colnames(data.by.ml), pattern = "windDirMean", ignore.case = T))
                dir.qf.index = as.numeric(grep(x=colnames(data.by.ml), pattern = "windDirFinalQF\\.", ignore.case = T))
                speed.indx <- as.numeric(grep(x=colnames(data.by.ml), pattern = "windSpeedMean", ignore.case = T))

                if(length(speed.indx)==0){stop("No wind data found!")}


                direct<-data.by.ml[,dir.indx]
                speed<-data.by.ml[,speed.indx]
                qf=paste0("QF=",data.by.ml[,dir.qf.index])


                temp.df<-data.frame(Date=data[,1], "Dir"=as.numeric(direct), "Speed"=as.numeric(speed), "QualityFlag"=qf, "ML"=rep(clean.mls[n], length(direct)))
                temp.df<-temp.df[temp.df$Dir>=0,]
                temp.df=temp.df[temp.df$Speed>0.5,]
                all<-rbind(all, temp.df)
            }

            #Factor by season

            all$Season[lubridate::month(all$Date) %in% seasons$winter]="Winter"
            all$Season[lubridate::month(all$Date) %in% seasons$summer]="Summer"
            all$Season[lubridate::month(all$Date) %in% seasons$spring]="Spring"
            all$Season[lubridate::month(all$Date) %in% seasons$fall]="Fall"

            all=all[!is.null(all),]
            all=all[!is.na(all),]

            all$Season=factor(x = all$Season, levels = c("Winter", "Spring", "Summer", "Fall"))

            if(as.character(qf.plot)=="0"){
                all=all[all$QualityFlag=="QF=0",]
            }
            if(as.character(qf.plot)=="1"){
                all=all[all$QualityFlag=="QF=1",]
            }

            # Set up output parameters for plot
            degreeSteps<-as.numeric(360/dir.bins)
            dir.bin.seq<-seq(0, 360, by=360/dir.bins)
            all.binned<-cbind(all, SpeedCut= cut(as.numeric(all$Speed), breaks = speed.bins, ordered_result = TRUE), DirCut= cut(as.numeric(all$Dir), breaks = dir.bin.seq))
            all.binned<-stats::na.omit(all.binned)
            all.binned$SpeedCut=factor(all.binned$SpeedCut, levels =  rev(levels(all.binned$SpeedCut)))
            #all.binned=all.binned[order(all.binned$SpeedCut, decreasing = T),]

            # Make labels and title
            bgnLabels<- unique((dir.bin.seq-(degreeSteps/2))%%360)
            endLabels<- unique((dir.bin.seq+(degreeSteps/2))%%360)
            dirLabels<-paste0(bgnLabels, "-", endLabels)
            titleString =paste0(domain, "-", site, " wind data from ", bgn.month, " through ", end.month)

            degreeLabels=endLabels

            degreeLabels[gtools::odd(1:36)]=""

            #Make and prettify the plot
            plot=ggplot2::ggplot(data = all.binned, ggplot2::aes(x=DirCut, fill=SpeedCut, colors=factor(SpeedCut)))+
                ggplot2::geom_bar(width = .95, show.legend = T)+
                ggplot2::theme_linedraw()+
                ggplot2::theme_light()+
                ggplot2::coord_polar(theta = "x", start = 0)+
                ggplot2::theme(legend.position="bottom", legend.title.align = (0.5), axis.text.x = ggplot2::element_text(angle=(85-(c(0:35)*(90/9)) %% 180)))+
                ggplot2::xlab("")+
                ggplot2::ylab("Count")+
                ggplot2::labs(title=titleString)+
                ggplot2::scale_x_discrete(labels=degreeLabels)+
                ggplot2::scale_fill_discrete(h = c(0, 240), l=65, c=100, name="Wind Speed (m/s)")+
                ggplot2::facet_grid(ML~Season, switch = "y")

            if(as.character(qf.plot)=="0"){
                plot=plot+ggplot2::ggtitle(label = titleString, subtitle = "Only showing records where QF=0")
            }
            if(as.character(qf.plot)=="1"){
                plot=plot+ggplot2::ggtitle(label = titleString, subtitle = "Only showing records where QF=1")
            }

            print(plot)

        }
    }
    dev.off()
}

