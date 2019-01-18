#collate NEON info from the API
setwd("~/Dropbox/NEON/site locations/")

# The info we have on the web
website.info=read.csv("./field-sites.csv", stringsAsFactors = F)
website.elev=as.numeric(gsub(pattern = " m", replacement = "", x = website.info$Elevation))

# Get API info
api.info=lapply(website.info$Site.ID, function(x) Z10::get.site.meta(site = x))
api.elev=round(unlist(lapply(api.info, "[[", "location.elevation")))
domains=unlist(lapply(api.info, "[[", "domain.code"))



elevations=data.frame(site=paste0(domains, "-", website.info$Site.ID), type=website.info$Site.Type, website=website.elev, api=api.elev, difference=abs(as.numeric(website.elev)-api.elev))

diff.elevs=elevations[-which(elevations$difference==0),]
diff.elevs=diff.elevs[order(diff.elevs$site),]


# LAT LON CHECK ####
# The info we have on the web
website.lat.lon=data.frame(do.call(rbind, stringr::str_split(pattern = ", ", string = website.info$Lat..Long.)))
website.lat.lon=sapply(website.lat.lon, trimws)


# Get API info
api.lat=unlist(lapply(api.info, "[[", "location.decimal.latitude"))
api.lon=unlist(lapply(api.info, "[[", "location.decimal.longitude"))
#domains=unlist(lapply(api.info, "[[", "domain.code"))

locations=data.frame(site=paste0(domains, "-", website.info$Site.ID), 
                     type=website.info$Site.Type, 
                     website.lat=website.lat.lon[,1],
                     website.lon=website.lat.lon[,2],
                     api.lat,
                     api.lon,
                     lat.difference=abs(as.numeric(website.lat.lon[,1])-api.lat),
                     lon.difference=abs(as.numeric(website.lat.lon[,2])-api.lon))

