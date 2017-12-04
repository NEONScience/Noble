#Make is_site_config
library(Noble)
TIS=tis_site_config
AIS=ais_site_config


TIS=data.frame(System=rep("TIS", times=length(TIS[,1])), TIS)
AIS=data.frame(System=rep("AIS", times=length(AIS[,1])), AIS)


is_site_config=data.frame(
    System=c(TIS$System, AIS$System),
    SiteID=c(TIS$SiteID, AIS$SiteID),
    Domain=c(TIS$Domain, AIS$Domain),
    Site.Type=c(rep(NA, times=length(TIS[,1])), AIS$Site.Type),
    Core.Relocatable=c(TIS$Core.Relocatable, AIS$Core.Relocatable)
)

save(is_site_config, file = "../Noble/data/is_site_config.rda")
