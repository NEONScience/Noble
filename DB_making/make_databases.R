#Make is_site_config

options(stringsAsFactors = FALSE)
library(Noble)
TIS=Noble::tis_site_config
AIS=Noble::ais_site_config

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

## Make the TIS_pri_vars DB, from the CSV

tis_pri_vars=data.frame(read.csv(file="../../DB_making/raw_DBs/tis_pri_vars.csv"))
save(tis_pri_vars, file = "./data/tis_pri_vars.rda")


## Make the wind thresholds, from the CSV

#wind_thresholds=data.frame(read.csv(file="../DB_making/raw_DBs/ATBD_thresholds_2Dwind.csv"))
wind_thresholds=data.frame(read.csv("../../DB_making/raw_DBs/Threshold_2D_Wind_Direction_REVB_constraint_2018-12-26T21_39_06.779Z.csv"), stringsAsFactors = F)
save(wind_thresholds, file = "./data/wind_thresholds.rda")


## Make the radiation DQ testing info DB
rad_dq_info=data.frame(read.csv(file="../DB_making/raw_DBs/rad_dq_info.csv"))
save(rad_dq_info, file = "../Noble/data/rad_dq_info.rda")


## Make the USCRN site data table
USCRN_sites = data.frame(read.csv("../DB_making/raw_DBs/uscrn_sites.csv", stringsAsFactors = F))
# write.csv(USCRN_sites, file = "../DB_making/raw_DBs/uscrn_sites.csv", row.names = F)
save(USCRN_sites, file="../Noble/data/uscrn_sites.rda")

#AIS Site config
ais_site_config=read.csv(file="../../DB_making/raw_DBs/ais_sites.csv")
colnames(ais_site_config)=tolower(colnames(ais_site_config))
save(ais_site_config, file = "./data/ais_site_config.rda")

#TIS Site Config
tis_site_config=read.csv("../../DB_making/raw_DBs/tis_site_config.csv", stringsAsFactors = F)
colnames(tis_site_config)=tolower(colnames(tis_site_config))
save(tis_site_config, file = "./data/tis_site_config.rda")
