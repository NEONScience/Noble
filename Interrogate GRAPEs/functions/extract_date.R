#file="/Volumes/neon/Common/ENG/Sites/Sensors/maps/HEAL/HEAL_20171108_130003.csv"

extractDate=function(file){
    broken=unlist(stringr::str_split(string=file, pattern = "/"))
    rawDate=unlist(stringr::str_split(string=broken[length(broken)], pattern = "_"))[2]
    date=as.Date(rawDate, format = "%Y%m%d")
    return(date)
}