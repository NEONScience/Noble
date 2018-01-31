test.dir="/Volumes/neon/Science/Science Commissioning Archive/SiteAndPayload/TisRadiationProcessQuality/"

PQ.results=read.csv(paste0(test.dir, "Common/summary_results.csv"))
test.sites=PQ.results[as.logical(PQ.results$passed),]
test.sites=test.sites[(test.sites$site %in% Noble::rad_dq_info$Site),]

save.dir="/Volumes/neon/Science/Science Commissioning Archive/SiteAndPayload/TisRadiationDataQuality/"
sink=lapply(test.sites$site, function(x) Noble::rad.dq.test(site = x, save.dir = save.dir, bgn.month = test.sites$bgn[test.sites$site==x], end.month = test.sites$end[test.sites$site==x]))
