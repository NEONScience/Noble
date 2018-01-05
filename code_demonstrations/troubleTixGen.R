# Trouble Ticket Script - PQ tests only
pq.dpID="DP1.00040.001"

testSubDir = Noble::tis_pri_vars$pq.sca.folder[Noble::tis_pri_vars$dpID==pq.dpID]

if(grepl("darwin", version$os))
{
    mount.point<-"/Volumes/neon/" #Mac
}else{
    mount.point<-"N:/" #Windows
}

dirCommBase <- paste0(mount.point, "Science/Science Commissioning Archive/SiteAndPayload/")
testFullDir = paste0(dirCommBase, testSubDir)
resultFile = paste0(testFullDir,"/Common/results.csv")

survey.dir=paste0(testFullDir, "/Common/troubleTickets/")
if(!dir.exists(survey.dir)){dir.create(survey.dir)}

results = data.frame(read.csv(resultFile, header=TRUE), stringsAsFactors = FALSE)

# Important! Only reads the most recent results data per site into the RMD #
siteList = (unique(results$site))
numbSites = as.numeric(length(siteList))

results=results[with(results, order(site, time_performed, data_product)),]

vars=unique(results$variable_tested)

parsed.results=data.frame()

for (k in 1:numbSites) {
    siteIndex=grep(pattern = siteList[k], results$site)
    siteOnly =results[siteIndex,]

    varList = (unique(siteOnly$variable_tested))

    for(v in 1:length(varList)){
        varOnly=siteOnly[which(siteOnly$variable_tested==varList[v]),]
        varOut=varOnly[which.max(as.POSIXct(varOnly$time_performed)),]
        parsed.results=rbind(parsed.results, varOut)
    }
}

quant.fail=parsed.results[parsed.results$data_quantity<parsed.results$quant_threshold,]
valid.fail=parsed.results[parsed.results$data_validity<parsed.results$valid_threshold&!(parsed.results$site[parsed.results$data_validity<parsed.results$valid_threshold] %in% quant.fail$site),]

quant.sites=unique(quant.fail$site)

#quant.report
for(i in 1:length(quant.sites)){
    site=quant.sites[i]
    bgn.month=quant.fail$begin_month[quant.fail$site==site][1]
    end.month=quant.fail$end_month[quant.fail$site==site][1]
    bgn.month=stringr::str_sub(bgn.month, 1, 7)
    end.month=stringr::str_sub(end.month, 1, 7)

    test.dates=paste0(quant.fail$begin_month[quant.fail$site==site][1], " through ", quant.fail$end_month[quant.fail$site==site][1])
    bad.dps=unique(quant.fail$data_product[quant.fail$site==site])
    dp.quant=quant.fail$data_quantity[quant.fail$site==site&quant.fail$data_product %in% bad.dps]
    dp.string=c()
    if(length(bad.dps)>1){
        for(i in 1:(length(bad.dps)-1)){
            dp.string=paste0(dp.string, bad.dps[i], " (", dp.quant[i], "%), ")
        }
        dp.string=paste0(dp.string, "and ", bad.dps[length(bad.dps)], " (", dp.quant[length(bad.dps)], "%) were below the testing threshold over the test period (", test.dates, ").")
    }else{dp.string=paste0(bad.dps[length(bad.dps)], " (", dp.quant[length(bad.dps)], "%) was below the testing threshold over the test period (", test.dates, ").")}

    title=paste0("TIS ", Noble::tis_pri_vars$kpi[Noble::tis_pri_vars$dpID==pq.dpID], " commissioning test anomaly @ ", Noble::tis_site_config$Domain[Noble::tis_site_config$SiteID==site], "-", site, ": Data Quantity insufficient")
    blurb=paste0("At ", site, ", commissioning testing resulted in a failure, due to low data quantity. ", dp.string, " Attached are summary figures of data product health, and CSVs showing when gaps in data begin and end.")
    stakeholders="rlee, gwirth, jcrow, lmorgan, nvandenhul"

    text.out=paste0("TITLE/n/n", title, "/n BODY/n", blurb, "/n STAKEHOLDERS /n", stakeholders)
    writeLines(c("TITLE", title, "", "BODY", blurb, "", "STAKEHOLDERS", stakeholders), con= paste0(Noble:::.data.route(site=site, save.dir = survey.dir), "/ticket.txt"))

    for(j in 1:length(unique(quant.fail$data_product[quant.fail$site==site]))){
        ## Clean this up for work in script. add if statement for 2D wind and SAAT
        dpID=unique(quant.fail$data_product[quant.fail$site==site])[j]

        short.name=Noble::tis_pri_vars$short.name[Noble::tis_pri_vars$dpID==dpID]
        data.field=Noble::tis_pri_vars$data.field[dpID==Noble::tis_pri_vars$dpID]

        try(Noble:::.pull.n.plot.png(sites.req = site, bgn.month = bgn.month, end.month = end.month, dpID = dpID, save.dir = Noble:::.data.route(site, survey.dir), data.field = data.field))
        try(Noble::gap.report(site=site, bgn.month = bgn.month, end.month = end.month, dpID = dpID, save.dir = Noble:::.data.route(site, survey.dir)))
        if(dpID=="DP1.00001.001"){

        }
        if(dpID=="DP1.00002.001"|dpID=="DP1.00003.001"){
            try(Noble::air.temp.cnst.plot(site=site, bgn.month = bgn.month, end.month = end.month, save.dir = Noble:::.data.route(site, survey.dir)))
            try(Noble::air.temp.plot(site=site, bgn.month = bgn.month, end.month = end.month,save.dir = Noble:::.data.route(site, survey.dir)))
        }
        try(Noble:::plot.dp.survey(dpID = dpID, save.dir = Noble:::.data.route(site, survey.dir), site = site))
        #paste0(site, "_", bgn.month, "-", end.month, "_", short.name, "_NO_DATA.csv")
    }
}
