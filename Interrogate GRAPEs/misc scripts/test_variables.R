site="CPER"
bgn="2017-07-01"
end="2017-07-31"



#Define tesing parameters
#col.names=c("Row", "ID", "Connections", "Uptime", "MAC")
eng.log=read.csv("/Volumes/neon/Common/ENG/Sites/Sensors/data/CPER/CPER_20180112_000101.csv", header = F, col.names = col.names, stringsAsFactors = F)
map=read.csv("/Volumes/neon/Common/ENG/Sites/Sensors/maps/CPER/CPER_20180112_130002.csv")

write.csv(x = read.csv("/Volumes/neon/Common/ENG/Sites/Sensors/data/CPER/CPER_20180112_000101.csv", header = F), file="./data/ex_log.csv", row.names = F)
write.csv(x = read.csv("/Volumes/neon/Common/ENG/Sites/Sensors/maps/CPER/CPER_20180112_130002.csv"), file="./data/ex_map.csv", row.names = F )

eng.log= read.csv(file="./data/ex_log.csv")
map= read.csv(file="./data/ex_map.csv")
 