health.data= function(site, dpID){

    pri.var=Noble::tis_pri_vars$data.field[which(Noble::tis_pri_vars$dpID==dpID)]
    var.name=gsub(pattern = "mean", replacement = "", x = pri.var, ignore.case = T)


    dp.avail = NEON.avail(dpID = dpID)
    dp.avail = cbind(Month=dp.avail[,1],  dp.avail[,which(colnames(dp.avail) %in% Noble::tis_site_config$SiteID)])

    message(paste0("Working on ", site, "..."))

    temp.dates = zoo::as.Date(dp.avail$Month[
        which(
            dp.avail[which(colnames(dp.avail)==site)]=="x"
        )
        ]
    )
    run.dates = substr(temp.dates, start = 0, stop = 7)

    health.data=data.frame(Month=run.dates, Availability=rep(0, times=length(run.dates)), Validity =rep(0, times=length(run.dates)))


    for(d in 1:length(run.dates)){
        message(paste0("Downloading ", run.dates[d]))
        month.data<-try(Noble::data.pull(site = site, dpID = dpID, bgn.month = run.dates[d], end.month = run.dates[d], time.agr = 30, package = "basic", save.dir = tempdir()))

        if(!length(month.data)==0){
            priData=data.frame(month.data[,grepl(pattern = pri.var, x = colnames(month.data), ignore.case = T)])
            if(!length(priData)==0){

                pcntData=round(sum(colSums(!is.na(priData)))/(length(priData[,1])*length(priData))*100, digits = 2)

                finalQFs=data.frame(month.data[,grepl(pattern = "*finalQF*", x = colnames(month.data), ignore.case = T)])

                if(length(colnames(finalQFs))>length(colnames(priData))){
                    finalQFs=data.frame(finalQFs[,grepl(pattern = var.name, x = colnames(finalQFs), ignore.case = T)])
                }

                pcntValid=round(100-(sum(colSums(finalQFs, na.rm = T))/(length(priData[,1])*length(priData))*100+
                                         sum(colSums(is.na(finalQFs)))/(length(priData[,1])*length(finalQFs))*100), digits = 2)


                health.data$Availability[which(health.data$Month==run.dates[d])]=pcntData
                health.data$Validity[which(health.data$Month==run.dates[d])]=pcntValid}
        }
    }
    return(health.data)
}
