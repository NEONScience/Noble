## Noble (public) unit testing

## General Parameters
site="CPER"
bgn.month="2018-09"
end.month=bgn.month
dp.id="DP1.00001.001"
save.dir="~/Desktop/unit-testing/"
package="basic"
time.agr=30

#make the save dir, as needed.
if(!dir.exists(save.dir)){dir.create(save.dir, recursive = T)}

# clean up existing file/log output from save.dir
file.remove(list.files(path = save.dir, full.names = T, recursive = T))

make.log=function(quoted.code, result){
    desc=try(unlist(lapply(evaluate::evaluate(quoted.code), "[[", "message")),silent = T)

    test=unlist(strsplit(quoted.code, split = "\\("))[1]
    x=paste0(test, ": ", result, "  (Tested: ", Sys.time(), ")")
    if(!class(desc)=="try-error"){x=paste0(x, "\n    ERRORS/warnings:\n", paste(" > ", desc, collapse = "\n"), "\n")}

    write(x=x,
          file = paste0(save.dir, "test_results.log"),
          append = T)
}

## Air temperature consistency plot
make.log(quoted.code = "Noble::air.temp.cnst.plot(site=site,
                          bgn.month = bgn.month,
         end.month = end.month,
         save.dir = save.dir)",
    result=if(file.exists(paste0(save.dir, "/", site, "_", bgn.month,"-", end.month,  "_ML_AT_comparisons.png"))){"PASS"}else{"FAIL"}
)


## Air temperature consistency test
quoted.code="Noble::air.temp.dq.test(site=site,
                              bgn.month = bgn.month,
                              end.month = end.month,
                              save.dir = save.dir)"
out=try(Noble::air.temp.dq.test(site=site,
                                bgn.month = bgn.month,
                                end.month = end.month,
                                save.dir = save.dir))

make.log(quoted.code = quoted.code,
         result=if(class(out)=="character"){"PASS"}else{"FAIL"}
         )


## pull.data
out=Noble::pull.data(site = site, dp.id = dp.id, bgn.month = bgn.month, end.month = end.month, time.agr = time.agr, package = package, save.dir = save.dir)
quoted.code="Noble::pull.data(site = site, dp.id = dp.id, bgn.month = bgn.month, end.month = end.month, time.agr = time.agr, package = package, save.dir = save.dir)"
make.log(quoted.code = quoted.code,
         result=if(is.data.frame(out)){"PASS"}else{"FAIL"})


## co2.pq.test
make.log(quoted.code = "Noble::co2.pq.test(site = site, bgn.month = bgn.month, end.month = end.month,  save.dir = save.dir)",
         result = if(file.exists(paste0(save.dir, "D10-CPER/co2Turb_CPER_2018-09-01-2018-09-30.csv"))){"PASS"}else{"FAIL"}
        )


data=Noble::pull.data(site = site, dp.id = dp.id, bgn.month = bgn.month, end.month = end.month, time.agr = time.agr, package = package, save.dir = save.dir)
out=Noble::date.extract(data = data, bgn.date = "2018-09-01", end.date = "2018-09-02")
make.log(quoted.code="Noble::date.extract(data = data, bgn.date = '2018-09-01', end.date = '2018-09-02')", result = if(is.data.frame(out)&nrow(out)>1){"PASS"}else{fail})

## distorted.fields
out=Noble::distorted.field(site)
quoted.code="Noble::distorted.field(site)"
make.log(quoted.code = quoted.code, result = if(class(out)=="list"&length(out)==3){"PASS"}else{"FAIL"})

## dp.survey - takes a long time to run
#quoted.code="Noble::dp.survey(dp.id=dp.id, save.dir = save.dir, site = 'YELL')"
#out=Noble::dp.survey(dp.id=dp.id, save.dir = save.dir, site = 'YELL')
#make.log(quoted.code = quoted.code, result = if(is.data.frame(out)){"PASS"}else{"FAIL"})

#last.day.time
out=Noble::last.day.time(end.month = end.month, time.agr = 30)
quoted.code="Noble::last.day.time(end.month = end.month, time.agr = 30)"
make.log(quoted.code = quoted.code, result = if(length(out)>0&any(class(out)=="POSIXt")){"PASS"}else{"FAIL"})

# fan.test
quoted.code="Noble::fan.test(site, bgn.month, end.month, save.dir)"
make.log(quoted.code, result = if(file.exists(paste0(save.dir, "/common/results.csv"))){"PASS"}else{"FAIL"})

# gap.find
out=Noble::gap.find(data=data, time.agr = 30, 'times')
quoted.code="Noble::gap.find(data=data, time.agr = 30, 'times')"
make.log(quoted.code = quoted.code, result = if(class(out)=="list"){"PASS"}else{"FAIL"})

# gap.report
quoted.code="Noble::gap.report(site = site, bgn.month = bgn.month, end.month = end.month, dp.id = dp.id, save.dir = save.dir)"
make.log(quoted.code = quoted.code, result = if(file.exists(paste0(save.dir, "CPER_2018-09-2018-09_2DWind_NO_DATA.csv"))){"PASS"}else{"FAIL"})

# gap.vis
quoted.code="Noble::gap.vis(site = site, bgn.month = bgn.month, end.month = end.month, dp.id = dp.id, save.dir = save.dir)"
make.log(quoted.code = quoted.code, result = if(file.exists(paste0(save.dir, "2DWind_CPER_Gaps_2018-09-2018-09.png"))){"PASS"}else{"FAIL"})

Noble::health.data
