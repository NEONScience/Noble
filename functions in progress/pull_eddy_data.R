
test.dir="/Volumes/neon/Science/Science Commissioning Archive/SiteAndPayload/T"

pull.eddy.data=function(site, bgn.month, end.month, package, save.dir){
    dp.data=jsonlite::read_json(path="http://data.neonscience.org/api/v0/products/DP4.00200.001/")$data
    deployed.sites=unlist(lapply(seq(length(dp.data$siteCodes)), function(x) dp.data$siteCodes[[x]]$siteCode))
    if(site %in% deployed.sites){
        avail.months=unlist(lapply(dp.data$siteCodes, function(x) if(x$siteCode==site){x$availableMonths}))
        avail.urls=unlist(lapply(dp.data$siteCodes, function(x) if(x$siteCode==site){x$availableDataUrls}))
    }else{stop(paste0("Bundled eddy covariance data products aren't available at ", site, " yet!"))}

    call.df=data.frame(avail.months=avail.months, avail.urls=avail.urls)

    requested.months=substr(seq.Date(from = as.Date(paste0(bgn.month, "-01")), to=as.Date(paste0(end.month, "-01")), by="month"), 1, 7)

    api.months=requested.months[requested.months %in% call.df$avail.months]
    if(length(api.months)==0){stop("No data found in specified date range.")}

    data.links=lapply(api.months, function(x) jsonlite::read_json(call.df$avail.urls[call.df$avail.months==x]))
    data.links=lapply(seq(length(data.links)), function(x) data.links[[x]]$data$files)[[1]]

    hdf5.links=unlist(lapply(data.links, function(x) if(grepl(pattern = ".zip", x = x$name)){x$url}))

    hdf5.links=hdf5.links[grepl(pattern = package, x = hdf5.links)]


    #Need to add in a smart way to include the month of the file requested in the file read/write
    # lapply(hdf5.links, function(x) download.file(url = x, destfile = paste0(save.dir, site, "_DP4.00200.001.zip")))
    #
    # utils::unzip(zipfile = paste0(save.dir, site, "_DP4.00200.001.zip"), exdir = save.dir)
    # out=rhdf5::h5read(file =
    #                   paste0(save.dir, site, "_DP4.00200.001/NEON.",
    #                          Noble::tis_site_config$Domain[Noble::tis_site_config$SiteID==site],
    #                          ".",site, ".DP4.00200.001.nsae.", api.months[1] ,".basic.h5"),
    #                     name = "CPER")
    #
    # #"NEON.D10.CPER.DP4.00200.001.nsae.2017-09.basic.h5"
    # return(out)

}

