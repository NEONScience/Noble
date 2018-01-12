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

# changelog and author contributions / copyrights
#   Robert Lee (2017-07-20)
#     original creation
#
##############################################################################################


tis.pq.test<-function(site = "CPER", dpID = "DP1.00001.001", prin.vars,  bgn.month = "2012-05", end.month = "2012-06", time.agr = 30, package="basic", save.dir, q.th=95, v.th=90)
{
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

    #Make domain-specific directory
    site.dir=Noble:::.data.route(site = site, save.dir = save.dir)

    #pull data
    test.data=data.frame()
    test.data=try(Noble::data.pull(site = site, dpID = dpID, bgn.month = bgn.month, end.month = end.month, time.agr = time.agr, package=package, save.dir=site.dir))
    if(length(test.data)>1){
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
                qf.indx<-grep(x=colnames(test.data), pattern=("priPrecipFinalQF\\."), ignore.case = T)
            }

            bgn.day=as.Date(paste0(bgn.month, "-01"))
            end.day=as.POSIXct(Noble::end.day.time(end.month = end.month, time.agr = 1440))

            days=round(difftime(end.day, bgn.day, units="days"), digits = 2)
            end.day=lubridate::round_date(end.day, "day")

            num.nas<-sum(is.na(test.data[,data.indx]))
            num.data<-sum(!is.na(test.data[,qf.indx]))

            data.quant<-round(100*(num.data/(num.nas+num.data)), digits = 2)

            num.qf.fail<-sum(test.data[,qf.indx]==1, na.rm=TRUE)
            num.qf.pass<-sum(test.data[,qf.indx]==0, na.rm = TRUE)
            num.qf.na<-sum(is.na(test.data[,qf.indx]))

            data.valid<-round(100*(num.qf.pass/(num.qf.pass+num.qf.fail+num.qf.na)), digits = 2)

            if(prin.vars[i]=="SHF"){
                #Soil heat flux specific values
                quant_threshold=95
                valid_threshold=(90-15.38)
            }
            if(prin.vars[i]=="soilTemp"){
                quant_threshold=94.6
                valid_threshold=89.87
            }
            ##### WRITE RESULTS
            dq.rslt<-data.frame(site=site,
                                time_performed=as.character(Sys.time()),
                                begin_month=bgn.month,
                                end_month=end.month,
                                days_tested=days,
                                data_product= dpID,
                                variable_tested=prin.vars[i],
                                data_quantity=data.quant,
                                data_validity=data.valid,
                                quant_threshold= quant_threshold,
                                valid_threshold=valid_threshold
            )

            if(file.exists(Noble:::.result.route(save.dir))){
                dq.rpt <- data.frame(read.csv(file = Noble:::.result.route(save.dir), header = T, stringsAsFactors = T))
                dq.rpt <- rbind(dq.rpt, dq.rslt)
                write.csv(x = dq.rpt, file = Noble:::.result.route(save.dir), row.names = F)
            }
            else{
                write.csv(x = dq.rslt, file = Noble:::.result.route(save.dir), col.names = T, row.names = F)
            }
        }
    }else{
        for(i in 1:length(prin.vars)){
        if(prin.vars[i]=="SHF"){
            #Soil heat flux specific values
            quant_threshold=95
            valid_threshold=(90-15.38)
        }
        if(prin.vars[i]=="soilTemp"){
            quant_threshold=94.6
            valid_threshold=89.87
        }

        bgn.day=as.Date(paste0(bgn.month, "-01"))
        end.day=as.POSIXct(Noble::end.day.time(end.month = end.month, time.agr = 1440))

        days=round(difftime(end.day, bgn.day, units="days"), digits = 2)
        ##### WRITE RESULTS
        dq.rslt<-data.frame(site=site,
                            time_performed=as.character(Sys.time()),
                            begin_month=bgn.month,
                            end_month=end.month,
                            days_tested=days,
                            data_product= dpID,
                            variable_tested=prin.vars[i],
                            data_quantity=0,
                            data_validity=0,
                            quant_threshold= quant_threshold,
                            valid_threshold=valid_threshold
        )

        if(file.exists(Noble:::.result.route(save.dir))){
            dq.rpt <- data.frame(read.csv(file = Noble:::.result.route(save.dir), header = T, stringsAsFactors = T))
            dq.rpt <- rbind(dq.rpt, dq.rslt)
            write.csv(x = dq.rpt, file = Noble:::.result.route(save.dir), row.names = F)
        }
        else{
            write.csv(x = dq.rslt, file = Noble:::.result.route(save.dir), col.names = T, row.names = F)
        }}
    }
}

