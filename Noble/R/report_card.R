year="2017"
site="CPER"
save.dir="~/Desktop/test/"

report.card=function(year, site, save.dir){

    months=as.character(seq(1, 12))
    months[nchar(months)==1]=paste0(0, months[nchar(months)==1])
    months=paste0(year, "-", months)

    month.info=data.frame(month=months, num.days=unlist(lapply(zoo::as.yearmon(months), function(m) lubridate::days_in_month(m))))
    month.info$weight=month.info$num.days/(sum(month.info$num.days))

    # Decide which DPs to test based on if it's there- if a DP has no site instances it will not be tested - duh.
    dp.install=Noble::tis_site_config[Noble::tis_site_config$SiteID==site,which(colnames(Noble::tis_site_config) %in% Noble::tis_pri_vars$dpID)]
    test.dps=colnames(dp.install[colSums(dp.install)>0])

    summary=lapply(test.dps, function(x) Noble::health.data(site = site, dpID = x, bgn.month = month.info$month[1], end.month =  month.info$month[12], save.dir = save.dir))

    names(summary)=test.dps

    overall.avail=lapply(summary, function(s) sum(s$Availability*month.info$weight))
    overall.valid=lapply(summary, function(s) sum(s$Validity*month.info$weight))
    temp.avail=do.call(rbind, overall.avail)
    temp.valid=do.call(rbind, overall.valid)
    temp.all=data.frame(Availability=round(temp.avail[,1], digits=2), Validity=round(temp.valid[,1], digits = 2))
    Product=unlist(lapply(rownames(temp.all), function(x) Noble::tis_pri_vars$dp.name[Noble::tis_pri_vars$dpID==x]))

    year.sum=data.frame(Product, temp.all)
    overall=c("Availability"=mean(year.sum$Availability), "Validity"=mean(year.sum$Validity))
out=list(Site=site, Overall=overall, Details=year.sum)
print(out)
return(out)
}
