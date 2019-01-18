#old.info=read.csv(file = "../../DB_making/rebasing/TISsiteConfig.csv", stringsAsFactors = F)
old.info=Noble::tis_site_config

api.info=lapply(old.info$SiteID, function(x) Z10::get.site.meta(site = x))

domains=unlist(lapply(api.info, "[[", "domain.code"))
elevation=unlist(lapply(api.info, "[[", "location.elevation"))
site.type=tolower(unlist(lapply(api.info, "[[", "site.type")))
domn.site=paste0(domains, "-", old.info$SiteID)
latitude=unlist(lapply(api.info, "[[", "location.decimal.latitude"))
longitude=unlist(lapply(api.info, "[[", "location.decimal.longitude"))
site.name=unlist(lapply(api.info, "[[", "site.name"))

new.info=old.info

new.info$Latitude=latitude
new.info$Longitude=longitude
new.info$elevation=elevation
new.info$domn.site=domn.site
new.info$Site.Long.Name=site.name

colnames(new.info)=tolower(colnames(new.info))

write.csv(new.info, file = "../../DB_making/raw_DBs/tis_site_config.csv", row.names = F)
