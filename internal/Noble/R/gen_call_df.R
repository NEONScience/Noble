#generate call.df

.gen.call.df=function(bgn.month, end.month, site=site, dpID=dpID, time.agr=time.agr, package=package){

    bgn_temp <- as.Date(paste0(bgn.month, "-01"), tz="UTC")
    end_temp <- as.Date(paste0(end.month, "-01"), tz="UTC")

    date_range<-substr(seq.Date(bgn_temp, end_temp, "month"), 0, 7)

    site_meta=rjson::fromJSON(file = paste0("http://data.neonscience.org/api/v0/sites/", site), unexpected.escape = "skip")$data

    prod_meta=rjson::fromJSON(file = paste0("http://data.neonscience.org/api/v0/products/", dpID), unexpected.escape = "skip")$data

    prod_indx=grep(site_meta$dataProducts, pattern = dpID)
    site_indx=grep(prod_meta$siteCodes, pattern = site)

    if(length(prod_indx)==0){
        stop(paste0(dpID, " is not currently available at ", site, " via the API."))
    }

    site_options=data.frame(avail_months=unlist(site_meta$dataProducts[[prod_indx]]$availableMonths), urls= unlist(site_meta$dataProducts[[prod_indx]]$availableDataUrls))

    #####

    # Stop if no data
    if(length(site_options$avail_months)==0){stop(paste0(dpID, " is missing at ", site))}

    all_data_urls <- unlist(unique(site_options$urls))

    #construct temporary API call urls
    url_index<-lapply(date_range, function(x) grep(pattern=x, all_data_urls))
    temp_data_urls<-all_data_urls[unlist(url_index)]

    if(length(temp_data_urls)==0){stop("Data was missing in specified date range at ", site, ". Check ", dpID, " avalability with NEON.avail")}

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

    if((dpID %in% exceptions)){ #Why, oh why does bio temp have to be different on the API
        url_list<-url_list[grepl(pattern = paste0(time.agr, "_min*"), x= url_list)]
    }else{
        url_list=url_list[grepl(pattern = paste0(time.agr,"min*"), x= url_list)|grepl(pattern = paste0(time.agr, "_min*"), x= url_list)] #should catch unknown exceptions
    }

    #Looking for location info
    loc_list_temp=stringr::str_extract(string=url_list, pattern = paste0(dpID, "\\.\\d\\d\\d\\.\\d\\d\\d\\.\\d\\d\\d"))
    loc_list=stringr::str_sub(loc_list_temp, start = 15, end = 21)
    if(all(is.na(loc_list_temp))){
        loc_list_temp=stringr::str_extract(string=url_list, pattern ="\\.\\d\\d\\d\\.\\d\\d\\d\\.\\d\\d\\d")
        loc_list=stringr::str_sub(loc_list_temp, start = 1, end = 8)
    }

    dp_list<-rep(dpID, times=length(loc_list))

    call.df<-as.data.frame(cbind(url_list, dp_list, loc_list))

    # Order the call.df by data product, then location
    call.df<-call.df[order(call.df$dp_list, call.df$url_list),]
    call.df=call.df[which(grepl(x=call.df$url_list, pattern=package)),] #Keep only our package type
    call.df=call.df[which(grepl(x=call.df$url_list, pattern="\\.csv")),] #Keep only CSVs
    call.df=call.df[which(!grepl(x=call.df$url_list, pattern="variables")),] #weed out varaible tables

    return(call.df)

}
