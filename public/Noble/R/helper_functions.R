#Helper Functions

# Puts free-range data into a tidy file structure by site.
.data.org=function(dir){

    site.extract=function(file.name){
        site=stringr::str_extract(
            string = stringr::str_extract(
                string = file.name, pattern = "\\.[:upper:][:upper:][:upper:][:upper:]\\."
            ), pattern = "[:upper:][:upper:][:upper:][:upper:]"
        )
        return(site)
    }

    data.files=list.files(dir)[grepl(pattern = "\\.csv\\.gz", x = list.files(dir))]
    for(i in 1:length(data.files)){
        site=site.extract(data.files[i])
        domn=Noble::tis_site_config$Domain[Noble::tis_site_config$SiteID==site]
        if(!dir.exists(paste0(dir, domn, "-", site))){
            dir.create(paste0(dir, domn, "-", site))}
        file.copy(from = paste0(dir, data.files[i]), to = paste0(dir, domn, "-", site))
        file.remove(paste0(dir, data.files[i]))
    }
}

#Route data and results to appropriate files
.data.route=function(site, save.dir){
    domn=Noble::tis_site_config$Domain[Noble::tis_site_config$SiteID==site]
    data.route=paste0(save.dir, "/", domn, "-", site, "/")
    if(!dir.exists(data.route)){dir.create(data.route)}
    return(data.route)
}

.result.route=function(save.dir){
    result.dir=paste0(save.dir, "/Common/")
    if(!dir.exists(result.dir)){dir.create(result.dir)}
    result.route=paste0(result.dir, "results.csv")
    return(result.route)
}

#generate call.df####
.gen.call.df=function(bgn.month, end.month, site=site, dp.id=dp.id, time.agr=time.agr, package=package){

    bgn_temp <- as.Date(paste0(bgn.month, "-01"), tz="UTC")
    end_temp <- as.Date(paste0(end.month, "-01"), tz="UTC")

    date_range<-substr(seq.Date(bgn_temp, end_temp, "month"), 0, 7)

    site_meta=rjson::fromJSON(file = paste0("http://data.neonscience.org/api/v0/sites/", site), unexpected.escape = "skip")$data

    prod_meta=rjson::fromJSON(file = paste0("http://data.neonscience.org/api/v0/products/", dp.id), unexpected.escape = "skip")$data

    prod_indx=grep(site_meta$dataProducts, pattern = dp.id)
    site_indx=grep(prod_meta$siteCodes, pattern = site)

    if(length(prod_indx)==0){
        stop(paste0(dp.id, " is not currently available at ", site, " via the API."))
    }

    site_options=data.frame(avail_months=unlist(site_meta$dataProducts[[prod_indx]]$availableMonths), urls= unlist(site_meta$dataProducts[[prod_indx]]$availableDataUrls))

    #####

    # Stop if no data
    if(length(site_options$avail_months)==0){stop(paste0(dp.id, " is missing at ", site))}

    all_data_urls <- unlist(unique(site_options$urls))

    #construct temporary API call urls
    url_index<-lapply(date_range, function(x) grep(pattern=x, all_data_urls))
    temp_data_urls<-all_data_urls[unlist(url_index)]

    if(length(temp_data_urls)==0){stop("Data were missing in specified date range at ", site, ". Check ", dp.id, " avalability with neon.avail")}

    #For found DPs, given the Kpi, pull hosted metadata via API
    #print(temp_data_urls)
    api_data<-lapply(as.list(temp_data_urls), function(x) as.list(rjson::fromJSON(file = as.character(x), unexpected.escape = "skip")))

    # build a list of URLs served by the API
    url_list<-c()
    i<-1
    for(i in 1:length(api_data)){
        tempList<-api_data[[i]]$data$files
        listLeng<-length(tempList)
        if(listLeng==0){break()}
        for(j in 1:listLeng){
            url_list<-append(url_list, tempList[[j]]$url)
        }
    }

    # Weed out XML links
    url_list=url_list[!(grepl(pattern = "xml", x= url_list))]

    #Try to handle name excpetions
    exceptions=c("DP1.00005.001", "DP1.00041.001")

    if((dp.id %in% exceptions)){ #Why, oh why does bio temp have to be different on the API
        url_list<-url_list[grepl(pattern = paste0(time.agr, "_min*"), x= url_list)]
    }else{
        url_list=url_list[grepl(pattern = paste0(time.agr,"min*"), x= url_list)|grepl(pattern = paste0(time.agr, "_min*"), x= url_list)] #should catch unknown exceptions
    }

    #Looking for location info
    loc_list_temp=stringr::str_extract(string=url_list, pattern = paste0(dp.id, "\\.\\d\\d\\d\\.\\d\\d\\d\\.\\d\\d\\d"))
    loc_list=stringr::str_sub(loc_list_temp, start = 15, end = 21)
    if(all(is.na(loc_list_temp))){
        loc_list_temp=stringr::str_extract(string=url_list, pattern ="\\.\\d\\d\\d\\.\\d\\d\\d\\.\\d\\d\\d")
        loc_list=stringr::str_sub(loc_list_temp, start = 1, end = 8)
    }

    dp_list<-rep(dp.id, times=length(loc_list))

    call.df<-as.data.frame(cbind(url_list, dp_list, loc_list))

    # Order the call.df by data product, then location
    call.df<-call.df[order(call.df$dp_list, call.df$url_list),]
    call.df=call.df[which(grepl(x=call.df$url_list, pattern=package)),] #Keep only our package type
    call.df=call.df[which(grepl(x=call.df$url_list, pattern="\\.csv")),] #Keep only CSVs
    call.df=call.df[which(!grepl(x=call.df$url_list, pattern="variables")),] #weed out varaible tables

    return(call.df)

}


# Private function for calculating the percent of wind measurements falling due to wind coming
# through the buffers on either side of the distorted flow field
.percent.buffer=function(site, bgn.month, end.month, save.dir){
    ## Read in the site info and threshold info:
    if(missing(save.dir)){save.dir=tempdir()}

    #get our data
    example.data=Noble::pull.data(site=site,
                                  dp.id = "DP1.00001.001",
                                  bgn.month = bgn.month,
                                  end.month = end.month,
                                  time.agr = 30,
                                  package = 'expanded',
                                  save.dir = save.dir)

    example.data=Noble::un.ml.ize(example.data)

    # ATBD CALCS
    # Section 5.1
    ## Distorted Flow Test

    ###General parameters

    DF.info=Noble::distorted.field(site=site)
    message("Site: ", site, ". DFF low: ", DF.info$distortedField[1], ", DFF high: ", DF.info$distortedField[2])

    if(DF.info$distortedField[1]>DF.info$distortedField[2]){
        DF1=data.frame(dir=example.data$windDirMean[which(data.table::between(example.data$windDirMean, lower =DF.info$buff_low[1], upper=360))])
        DF2=data.frame(dir=example.data$windDirMean[which(data.table::between(x = example.data$windDirMean, lower =0, upper=DF.info$distortedField[2]))])
        DF=rbind(DF1, DF2)
    }else{
        DF=data.frame(dir=example.data$windDirMean[which(data.table::between(example.data$windDirMean, lower =DF.info$distortedField[1], upper=DF.info$distortedField[2]))])
    }
    LB_dir=example.data$windDirMean[which(data.table::between(example.data$windDirMean, lower = buff_low[1], upper=buff_low[2]))]
    UB_dir=example.data$windDirMean[which(data.table::between(example.data$windDirMean, lower = buff_hi[1], upper=buff_hi[2]))]

    pcnt.in.buffer=(length(UB_dir)+length(LB_dir))/length(example.data$windDirMean)*100
    message(paste0("Finshed with ", site))
    return(pcnt.in.buffer)
}


# Private function for calculating the percent of wind measurements falling due to wind coming
# through the buffers on either side of the distorted flow field
###
.percent.distorted=function(site, bgn.month, end.month, save.dir){

    if(missing(save.dir)){save.dir=tempdir()}

    #get our data
    example.data=Noble::pull.data(site=site,
                                  dp.id = "DP1.00001.001",
                                  bgn.month = bgn.month,
                                  end.month = end.month,
                                  time.agr = 30,
                                  package = 'expanded',
                                  save.dir = save.dir)

    example.data=Noble:::.un.ml.ize(example.data)

    DF.info=Noble::distorted.field(site=site)
    message("Site: ", site, ". DFF low: ", DF.info$distortedField[1], ", DFF high: ", DF.info$distortedField[2])
    if(DF.info$distortedField[1]>DF.info$distortedField[2]){
        DF1=data.frame(dir=example.data$windDirMean[which(data.table::between(example.data$windDirMean, lower =DF.info$distortedField[1], upper=360))])
        DF2=data.frame(dir=example.data$windDirMean[which(data.table::between(x = example.data$windDirMean, lower =0, upper=DF.info$distortedField[2]))])
        DF=rbind(DF1, DF2)
    }else{
        DF=data.frame(dir=example.data$windDirMean[which(data.table::between(example.data$windDirMean, lower =DF.info$distortedField[1], upper=DF.info$distortedField[2]))])
    }
    #UB_dir=example.data$windDirMean.000.010[which(data.table::between(example.data$windDirMean.000.010, lower = buff_hi[1], upper=buff_hi[2]))]

    DFF_plus_B=c(lower=(DF.info$distortedField[1]-10), upper=(DF.info$distortedField[2])+10)

    if(DFF_plus_B[1]>DFF_plus_B[2]){
        BDF1=data.frame(dir=example.data$windDirMean[which(data.table::between(example.data$windDirMean, lower =(DFF_plus_B[1]), upper=360))])
        BDF2=data.frame(dir=example.data$windDirMean[which(data.table::between(x = example.data$windDirMean, lower =0, upper=DFF_plus_B[2]))])
        BDF=rbind(DF1, DF2)
    }else{
        BDF=data.frame(dir=example.data$windDirMean[which(data.table::between(example.data$windDirMean, lower =DFF_plus_B[1], upper=DFF_plus_B[2]))])
    }
    All=(length(BDF[,1]))/length(example.data$windDirMean)*100
    DF.field.only=(length(DF[,1]))/length(example.data$windDirMean)*100
    out=c(All, DF.field.only)
    return(out)
}

#Average individual tower-based measurements accross all MLs
.un.ml.ize=function(data, keep.na){
    if(missing(keep.na)){keep.na=T}
    if(!is.logical(keep.na)){message("keep.na is not logical, defaulting to TRUE.")
        keep.na=T}
    data=data[,-2]
    data.melt=reshape2::melt(data, id.vars="startDateTime")
    data.melt$variable=stringr::str_replace(string = data.melt$variable, pattern = "\\.\\d\\d\\d\\.\\d\\d\\d", replacement = "")
    if(keep.na==F){data.melt=data.melt[-is.na(data.melt$value),]}

    data.out=reshape2::dcast(data = data.melt, formula = startDateTime~variable, fun.aggregate = mean)
    return(data.out)
}

#writes results files out
.write.results=function(result, save.dir){
    if(file.exists(Noble:::.result.route(save.dir))){
        temp.result = data.frame(read.csv(file = Noble:::.result.route(save.dir), header = T, stringsAsFactors = F))
        dq.rpt = rbind(temp.result, result)
        write.csv(x = dq.rpt, file = Noble:::.result.route(save.dir), row.names = F)
    }
    else{
        write.csv(x = result, file = Noble:::.result.route(save.dir), col.names = T, row.names = F)
    }
}

############################################################################################
# title  Returns PDF(s) of data for the specified site and data product

# author Robert Lee \email{rlee@battelleecology.org}\cr
# author Cove Sturdevant

# description For a specified data product ID, a data frame of the availabilty of that product
# for all NEON instrumented sites is returned. The output of data product availability is best
# interpreted with the base \code{View()} function.
#
# param \code{sites.req} The site, or character list of sites to return plots of.
# param \code{bgn.month} The start month to plot data for.
# param \code{end.month} The end month to plot data for.
# param \code{dp.id} Parameter of class character. The NEON data product code of the data product of interest.
# param \code{save.dir} The directory for data files and output PDFs to be saved to.
# param \code{data.field} Optional. The name of the measurement vaiable to plot. Defaults to the 'core' measurement for most products.

# return Outputs a a PDF of plots data on of all measurement levesl, with one PDF per site.

# keywords process quality, data quality, gaps, commissioning

# examples
# # for a variable, "test.dir", holding a valid file path:
# Noble:::.pull.n.plot.png(bgn.month = "2017-04", end.month = "2017-05", dp.id = "DP1.00001.001", sites.req = "BLAN", save.dir = getwd(), data.field = "windDirMean")

# seealso Currently none

# changelog and author contributions / copyrights
#   Robert Lee (2016-11-07)
#     original creation from Cove's code
#
#   Robert Lee (2017-07-17)
#     Updating function for Noble integration
#
#   Robert Lee (2017-12-27)
#     Branching off a new function to produce PNGs only
#
##############################################################################################

.pull.n.plot.png <- function(sites.req, bgn.month, end.month, dp.id, save.dir, data.field, package){
    #require(nneo)
    require(lubridate)

    #kpiList <- data.frame(read.csv("https://raw.githubusercontent.com/rhlee12/Data-Products/master/kpiList.csv", header = TRUE))
    time.agr=30


    test.qf = "finalQF"
    #ENTRY CONTROL
    #auto fill the data field, if not specified
    if(missing(data.field)){
        data.field = Noble::tis_pri_vars$data.field[Noble::tis_pri_vars$dp.id==dp.id]
    }

    if(interactive()&length(data.field)==0){
        data.field = readline(prompt = "No valid data field found, please enter one: ")
    }else if(!interactive()){stop("No valid data.field found, please enter one.")}

    if(missing(package)){
        package<-"basic"
    }

    pack.ctrl<-c("basic", "expanded")

    if(!package %in% pack.ctrl){
        package<-"basic"
        message("Invalid package type requested, defaulting to basic.")
    }

    DateBgn <- paste0(bgn.month, "-01")
    end_temp <- as.Date(paste0(end.month, "-01"), tz="UTC")
    end_temp<- as.POSIXlt(paste0(end_temp, "-01"), tz="UTC")
    end_temp$mon<-end_temp$mon+1
    end_temp<-end_temp-lubridate::minutes(time.agr)-lubridate::seconds(1)
    DateEnd<-as.Date(end_temp)

    s<-1
    for (s in 1:length(sites.req)){

        domn<-Noble::tis_site_config$Domain[Noble::tis_site_config$SiteID==sites.req[s]]
        dataFile<- paste0("NEON.", domn,".", sites.req[s],".", dp.id, "_REQ_", DateBgn, "_", DateEnd, "_", time.agr,"min_", package, ".csv.gz")
        fullPath <- paste0(save.dir, "/", dataFile)
        if (!file.exists(fullPath)){
            print(paste("Currently downloading data for:", sites.req[s]))

            sink<-Noble::pull.data(site = sites.req[s], bgn.month = bgn.month, end.month = end.month,
                                   dp.id = dp.id, time.agr = time.agr, package = package, save.dir = save.dir)
        }
        rm(sink) # get rid of environment data

        if(is.null(data.field)){stop("No data.field specified or identifiable. Specify this parameter for this DP ID.")}

        # Read the requested data back in
        print(paste("Reading and plotting", sites.req[s], "data."))
        commData <- data.frame(read.csv(fullPath, header = TRUE))

        if(dp.id=="DP1.00024.001"){
            commData=commData[,-which(grepl(pattern = "outPAR*", x = colnames(commData)))]
        }

        QFindex <- grep(pattern = "*finalQF\\.", colnames(commData), ignore.case = T)
        if(dp.id=="DP1.00001.001"){
            if(grepl(pattern = "*speed*", data.field, ignore.case = T)){
                QFindex = grep(pattern = "windSpeedFinalQF\\.", colnames(commData), ignore.case = T)
            }else if(grepl(pattern = "*dir*", data.field, ignore.case = T)){
                QFindex = grep(pattern = "windDirFinalQF\\.", colnames(commData), ignore.case = T)
            }
        }
        dataIndex <- grep(paste0(data.field), colnames(commData), ignore.case = T)
        timeStmp <- as.POSIXct(strptime(commData[,1], format="%Y-%m-%d %H:%M:%S", tz="UTC"))

        #{pdf(file=paste0(save.dir, "/", sites.req[s], "_", dp.id, "_", package, "_", data.field, ".pdf", sep=""), paper = "us")}

        niceColors<- c("0"="#41f299", "1"="#f25841", "NA"="black")

        for (idxPlot in 1:length(dataIndex)){

            nameData <- names(commData)[dataIndex[idxPlot]]
            data <- base::data.frame(time=timeStmp,data=commData[[dataIndex[idxPlot]]],qf=commData[[QFindex[idxPlot]]],
                                     nameData=nameData)

            nameQf=test.qf

            # Generate data completeness plot
            dataPlot <- base::data.frame(time=data$time,value=data$data,qfFail=data$qf)


            if(base::sum(data[[2]],na.rm=TRUE) == 0) {
                # No non-NA data, generate empty plot
                plotData <- ggplot2::ggplot(data=data,ggplot2::aes(x=time)) + ggplot2::geom_blank()
                #grobData <- ggplot2::ggplotGrob(plotData)
            } else {
                # Data to plot!

                plotData <- ggplot2::ggplot(data=data,ggplot2::aes(x=time,y=data)) +
                    ggplot2::geom_line() +
                    ggplot2::geom_point(size=1) +
                    ggplot2::geom_point(data=dataPlot,ggplot2::aes(x=time, y=value, color=factor(qfFail))) +
                    ggplot2::scale_color_manual(values = niceColors, name = "Final Quality Flag", limits=c(0, 1, "NA"))+
                    #ggplot2::scale_colour_continuous(low = "#a50037", high = "#00a560")+
                    ggplot2::theme_bw() +
                    ggplot2::labs(x="Date/Time", y=nameData, title=sites.req[s])
                # grobData <- ggplot2::ggplotGrob(plotData) # grab the grob for this plot for later manipulation
            }
            ggplot2::ggsave(filename =paste0(sites.req[s], "_", dp.id, "_", package, "_", nameData, ".png"), plot = plotData, device = "png", path = save.dir, width = 6, height = 3, units = "in")
            #gridExtra::grid.arrange(grobData,nrow=1) # plot it

        } ##Plotting code
        {graphics.off()}
        print(paste(sites.req[s], "complete."))
    }
}

# bgn.month="2017-06"
# end.month="2017-08"
# site="GRSM"
.wind.qm.summary=function(site, bgn.month, end.month, save.dir){
    if(missing(save.dir)){save.dir=tempdir()}
    data<-data.pull(site = site,
                    dp.id = "DP1.00001.001",
                    bgn.month = bgn.month,
                    end.month = end.month,
                    time.agr = 30,
                    package="expanded",
                    save.dir = save.dir)

    dir.QM.only=data[,(grepl(colnames(data), pattern = "windDir") & grepl(x=colnames(data), pattern = "QM"))]

    qm.sums=data.frame(startDateTime=NA,rbind(colSums(dir.QM.only>20, na.rm=T)))
    qm.count.avgs=Noble:::.un.ml.ize(qm.sums)
    qm.count.percent=round(qm.count.avgs/length(data[,1])*100, digits = 2)
    qm.count.percent=qm.count.percent[,-1]
    qm.count.percent=data.frame(Site=site, qm.count.percent)
    return(qm.count.percent)
}

#saves a plot of flow metrics, and returns the percent of records with good flow (percent of flow metrics where flowPassQM=100)

.flow.effect=function(site, bgn.month, end.month, save.dir)
{
    library(ggplot2)
    domn=Noble::tis_site_config$Domain[Noble::tis_site_config$SiteID==site]
    data.dir=.data.route(site=site, save.dir = save.dir)
    data=Noble::pull.data(site=site, dp.id = "DP1.00002.001", bgn.month = bgn.month, end.month = end.month, package = 'expanded', time.agr = 30, save.dir = data.dir)

    flow.flags=data.frame(Date=data[,1], data[grepl(pattern = "flow", x = colnames(data), ignore.case = T)])
    #colnames(flow.flags)=gsub(pattern = "\\.||\\d", replacement = "", x = colnames(flow.flags))

    melt.flow=reshape2::melt(flow.flags, id.vars="Date", na.rm=T)
    melt.flow$variable=gsub(pattern = "\\.||\\d", replacement = "", x = melt.flow$variable)
    melt.flow$Date=as.POSIXct(melt.flow$Date)


    niceColors<- c("flowFailQM"="#f25841", "flowNAQM"="grey", "flowPassQM"="#41f299")

    # melt.flow=reshape2::melt(data=flags.only[grepl(pattern = "flow", x = colnames(flags.only), ignore.case = T)]

    #flags=ggplot(melt.flags, aes(x = Date, y = value, fill=factor(variable)))+geom_bar(stat = 'identity')

    plot=ggplot(melt.flow, ggplot2::aes(x=Date, y = value/3, fill=factor(variable)))+
        theme_bw()+
        geom_bar(stat = 'identity', width = 10000)+
        scale_y_continuous(limits = c(0,100))+
        theme(axis.text.x =element_blank())+
        scale_fill_manual(name = "Flow Quality Metrics", values=niceColors)+
        labs(x=paste0(zoo::as.yearmon(bgn.month, format="%Y-%m"), " to ", zoo::as.yearmon(end.month, format="%Y-%m")), y="Flagging distribution (% of measuremnts)", title=paste0(domn, "-", site))

    ggsave(filename = paste0(site, "_", bgn.month, "-", end.month, "_flow.png"), path = paste0(save.dir, "/"), plot = plot, device = 'png', width = 5, height = 3.5, units = "in", dpi=300)

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
# param \code{ml} Optional. Used to specifiy what measurement level should be plotted.
# param \code{speed.bins} Optional. The number of bins for wind speed to be plotted in.
# param \code{dir.bins} Optional. The number of bins for wind directions to be plotted in.
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

.plot.qf.wind.rose = function(site, bgn.month, end.month, ml, speed.bins, dir.bins){
    time.agr = 30
    ml.case=missing(ml)

    # Massage dates for checking
    bgn_temp <- as.Date(paste0(bgn.month, "-01"), tz="UTC")
    end_temp <- as.Date(paste0(end.month, "-01"), tz="UTC")
    end_temp<- as.POSIXlt(paste0(end_temp, "-01"), tz="UTC")
    end_temp$mon<-end_temp$mon+1
    end_temp<-end_temp-lubridate::minutes(time.agr)-lubridate::seconds(1)

    # Return site metadata and products
    #site_meta<-nneo::nneo_site(site)

    # Warn about being too ambitious with this
    if(as.numeric(difftime(end_temp, bgn_temp))>=92){message("More than 3 months of data requested, may take a long time...")}

    # Return data
    data<-data.pull(site = site, dp.id = "DP1.00001.001", bgn.month = bgn.month, end.month = end.month, time.agr = 30, package="basic", save.dir = tempdir())

    # Set default bin breakdowns
    if(missing(speed.bins)){speed.bins=10}
    if(missing(dir.bins)){dir.bins=36}

    # Break out what MLs were returned
    temp<-(strsplit(colnames(data[,3:length(colnames(data))]), split = "\\."))
    temp=do.call(rbind, temp)
    #temp<-data.frame(temp[-1], stringsAsFactors=F)
    #temp<-unlist((temp[3,]))
    mls<-unique(temp[,3])
    clean.mls<-gsub(x=mls, pattern = "0", replacement = "")

    # If ML was specified, make sure it was in the found MLs, and then make sure only it is written
    if(!missing(ml)){
        if(!paste0("0", ml, "0") %in% mls){
            message("Specified ML not avaiable at this site. Pick from:")
            stop(paste(unlist(clean.mls)))
        }
        mls<-ml
    }

    # Build a DF of relevant data and MLS associated
    all<-NULL
    for(n in 1:length(mls)){
        data.by.ml<- data[,which(grepl(colnames(data), pattern=mls[n]))]

        dir.indx<- as.numeric(grep(x=colnames(data.by.ml), pattern = "windDirMean", ignore.case = T))
        dir.qf.index = as.numeric(grep(x=colnames(data.by.ml), pattern = "windDirFinalQF\\.", ignore.case = T))
        speed.indx <- as.numeric(grep(x=colnames(data.by.ml), pattern = "windSpeedMean", ignore.case = T))

        if(length(speed.indx)==0){stop("No wind data found!")}


        direct<-data.by.ml[,dir.indx]
        speed<-data.by.ml[,speed.indx]
        qf=paste0("QF=",data.by.ml[,dir.qf.index])


        temp.df<-as.data.frame(cbind("Dir"=as.numeric(direct), "Speed"=as.numeric(speed), "QualityFlag"=qf, "ML"=rep(mls[n], length(direct))))
        temp.df<-temp.df[temp.df$Dir>=0,]
        all<-rbind(all, temp.df)
    }
    all<-all[!is.null(all)]

    # Set up output parameters for plot
    degreeSteps<-as.numeric(360/dir.bins)
    dir.bin.seq<-seq(0, 360, by=360/dir.bins)
    all.binned<-cbind(all, SpeedCut= cut(as.numeric(all$Speed), breaks = speed.bins), DirCut= cut(as.numeric(all$Dir), breaks = dir.bin.seq))
    all.binned<-stats::na.omit(all.binned)

    # Make labels and title
    bgnLabels<- unique((dir.bin.seq-(degreeSteps/2))%%360)
    endLabels<- unique((dir.bin.seq+(degreeSteps/2))%%360)
    dirLabels<-base::paste0(bgnLabels, "-", endLabels)
    if(ml.case){titleString =base::paste0(site, " wind data from ", bgn.month, " through ", end.month)}else{
        titleString<-base::paste0("ML", ml, "-", site, " wind data from ", bgn.month, " through ", end.month)
    }
    #Make and prettify the plot
    plot<-ggplot2::ggplot(data = all.binned, ggplot2::aes(x=DirCut, fill=SpeedCut, colors=factor(SpeedCut)))+
        ggplot2::geom_bar(width = .95, show.legend = T)+
        ggplot2::theme_linedraw()+
        ggplot2::coord_polar(theta = "x", start = 0)+
        ggplot2::xlab("")+
        ggplot2::ylab("Count")+
        ggplot2::labs(title=titleString)+
        ggplot2::scale_x_discrete(labels=endLabels)+
        ggplot2::scale_fill_discrete(h = c(0, 240), l=65, c=100, name="Wind Speed, m/s")+
        ggplot2::facet_wrap(~QualityFlag)

    # If we didn't get ml specified, make a faceted plot
    if(ml.case){
        plot<-plot+ggplot2::facet_grid(QualityFlag~ML)
    }
    return(plot)
}





