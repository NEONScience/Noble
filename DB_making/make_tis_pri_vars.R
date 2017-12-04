
## Make the TIS_pri_vars DB, from the CSV

tis_pri_vars=data.frame(read.csv(file="../raw_DBs/tis_pri_vars.csv"))
save(tis_pri_vars, file = "../Noble/data/tis_pri_vars.rda")
