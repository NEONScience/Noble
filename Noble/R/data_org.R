# Puts data into a tidy file structure by site.

data.org=function(dir){

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
