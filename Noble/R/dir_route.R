.data.route=function(site, save.dir){
    domn=Noble::tis_site_config$Domain[Noble::tis_site_config$SiteID==site]
    data.route=paste0(save.dir, "/", domn, "-", site, "/")
    if(!dir.exists(data.route)){dir.create(data.route)}
    return(data.route)
}

.result.route=function(site, save.dir){
    domn=Noble::tis_site_config$Domain[Noble::tis_site_config$SiteID==site]
    result.dir=paste0(save.dir, "/Common/")
    if(!dir.exists(result.dir)){dir.create(result.dir)}
    result.route=paste0(result.dir, "results.csv")
    return(result.route)
}
