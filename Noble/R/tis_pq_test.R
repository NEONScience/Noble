############################################################################################
#' @title  Downloads and performs process quality checks on NEON data

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description For the specified dates, site, variables, and data product or name of family of data products,
#' data are downloaded and saved to the specifed directory. Process quality calculations are then performed and written to a results file in save.dir.
#'
#' @param \code{site} Parameter of class character. The NEON site data should be downloaded for.
#' @param \code{dpID} Parameter of class character. The name of the data product to pull data, or a
#' keyword for a family of data products, e.g. "wind" will pull for 2D and 3D wind data products.
#' @param \code{prin.vars} The principle variables to test (variable names, such as 'windSpeed'). Omit the term 'Mean'.
#' @param \code{bgn.month} Parameter of class character. The year-month (e.g. "2017-01") of the first month to get data for.
#' @param \code{end.month} Parameter of class character. The year-month (e.g. "2017-01") of the last month to get data for.
#' @param \code{time.agr} Parameter of class numeric. The data agregation interval requested, must be 1, 2, or 30.
#' @param \code{package} Parameter of class character. Optional. The type of data package to be returned If not specified, defaults to basic.
#' @param \code{save.dir} Parameter of class character. The local directory where data files should be saved.
#'
#' @return Writes data files to the specified directory.

#' @keywords process quality, data quality, gaps, commissioning

#' @examples
#' site = "CPER"
#' dpID = "DP1.00001.001"
#' prin.vars<-c("windSpeed", "windDir")
#' bgn.month = "2017-05"
#' end.month = "2017-06"
#' time.agr = 30
#' package="basic"
#' save.dir<-"/Users/rlee/Dropbox/GitHub/Commissioning-TIS-rhlee12/Tis2DWindPQ_test"
#' Noble::tis.pq.test(site = site, dpID = dpID, bgn.month = bgn.month, end.month = end.month,
#' time.agr = time.agr, package=package, save.dir=save.dir, prin.vars=prin.vars)

#'
#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Robert Lee (2017-07-20)
#     original creation
#
##############################################################################################


tis.pq.test<-function(site = "CPER", dpID = "DP1.00001.001", prin.vars,  bgn.month = "2017-05", end.month = "2017-06", time.agr = 30, package="basic", save.dir, q.th=95, v.th=90){

    quant_threshold=q.th
    valid_threshold=v.th

    if(missing(q.th)){
        q.th=95
        quant_threshold=q.th
    }
    if(missing(v.th)){
        v.th=90
    valid_threshold=v.th
    }


    domn=Noble::tis_site_config$Domain[which(Noble::tis_site_config$SiteID==site)]
    site.dir=paste0(save.dir, "/", domn, "-", site, "/")

    if(!dir.exists(site.dir)){
        dir.create(site.dir)
    }

    test.data<-Noble::data.pull(site = site, dpID = dpID, bgn.month = bgn.month, end.month = end.month, time.agr = time.agr, package=package, save.dir=site.dir)

    for(i in 1:length(prin.vars)){
        data.indx<-grep(x=colnames(test.data), pattern=paste0("^", prin.vars[i], "Mean*"))

        qf.indx<-grep(x=colnames(test.data), pattern=paste0("^", prin.vars[i], "FinalQF*"))
        qf.indx<-append(qf.indx, grep(x=colnames(test.data), pattern="^finalQF*"))

        if(prin.vars[i]=="inSW"){
            data.indx=data.indx[-which(grepl(x=colnames(test.data[,data.indx]), pattern = "003.000"))]
            qf.indx=qf.indx[-which(grepl(x=colnames(test.data[,qf.indx]), pattern = "003.000"))]
        }
        #special case for precip
        if(prin.vars[i]=="priPrecipBulk"){
            data.indx<-grep(x=colnames(test.data), pattern=paste0("^", prin.vars[i]))
            qf.indx<-grep(x=colnames(test.data), pattern=("FinalQF"), ignore.case = T)
        }


        bgn.day=as.Date(paste0(bgn.month, "-01"))
        end.day=Noble::end.day.time(end.month = end.month, time.agr = 1440)
        end.day=as.POSIXct(end.day)


        days=round(difftime(end.day, bgn.day, units="days"), digits = 2)
        end.day=lubridate::round_date(end.day, "day")

        pq.data<-test.data[,data.indx]
        qf.data<-test.data[,qf.indx]

        num.nas<-sum(is.na(pq.data))
        num.data<-sum(!is.na(pq.data))

        data.quant<-round(100*(num.data/(num.nas+num.data)), digits = 2)

        num.qf.fail<-sum(qf.data==1, na.rm=TRUE)
        num.qf.pass<-sum(qf.data==0, na.rm = TRUE)
        num.qf.na<-sum(is.na(qf.data))

        data.valid<-round(100*(num.qf.pass/(num.qf.pass+num.qf.fail+num.qf.na)), digits = 2)


#Set passing thresholds, based on var tested. Add to this area as functions or conditions are added
        ## direct radiation has an ATBD implementation error- revert to full thresholds.
        # if(prin.vars[i]=="dirRad"){
        #     #direct and diffuse caluculated values
        #     quant_threshold=Noble::dirRad.threshold(site = site, bgn.month = bgn.month, end.month = end.month, excuse = 5)
        #     valid_threshold=Noble::dirRad.threshold(site = site, bgn.month = bgn.month, end.month = end.month, excuse = 10)
        #}
    if(prin.vars[i]=="SHF"){
            #Soil heat flux specific values
            quant_threshold=95
            valid_threshold=(90-15.38)
        }
        if(prin.vars[i]=="soilTemp"){
            quant_threshold=94.6
            valid_threshold=89.87
        }


        dq.rslt<-data.frame(site, time_performed=as.character(Sys.time()), begin_month=bgn.month, end_month=end.month, days_tested=days, data_product= dpID, variable_tested=prin.vars[i], data_quantity=data.quant, data_validity=data.valid, quant_threshold= quant_threshold, valid_threshold=valid_threshold)

        rslt.dir=paste0(save.dir, "/", "Common/")
        if(!dir.exists(rslt.dir)){
            dir.create(rslt.dir)
        }

        if(file.exists(paste(rslt.dir,"results.csv",sep = "/"))){
            dq.rpt <- read.csv(file = paste(rslt.dir,"results.csv",sep = "/"), header = T, stringsAsFactors = T)
            dq.rpt <- rbind(dq.rpt, dq.rslt)
            write.csv(x = dq.rpt, file = paste(rslt.dir,"results.csv",sep = "/"), row.names = F)
        }
        else{
            write.csv(x = dq.rslt, file = paste(rslt.dir,"results.csv",sep = "/"), col.names = T, row.names = F)
        }


    }
}
