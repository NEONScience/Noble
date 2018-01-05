bgn.month="2017-06"
end.month="2017-08"

tis_test_sites=c("HARV", "BART", "SERC", "SCBI", "BLAN", "OSBS", "JERC", "DSNY", "LAJA", "GUAN",
                 "UNDE", "UKFS", "KONZ", "KONA", "ORNL", "MLBS", "GRSM", "TALL", "LENO", "DELA",
                 "WOOD", "TREE", "NOGP", "DCFS", "STER", "RMNP", "CPER", "OAES", "CLBJ", "NIWO",
                 "MOAB",  "SRER", "JORN", "ONAQ", "ABBY", "SJER", "BARR", "TOOL", "DEJU", "HEAL")

passed.sites=c()
bad.sites=c("ABBY", "SJER", "NOGP", "SRER")

test.sites=tis_test_sites[which(!tis_test_sites %in% passed.sites)] #filter out sites that passed testing
test.sites=test.sites[-which(test.sites %in% bad.sites)]

raw.qm=lapply(test.sites, function(x) Noble:::.wind.qm.summary(site = x, bgn.month = bgn.month, end.month = end.month))

qm.df=do.call(rbind, raw.qm)

bad.qm=data.frame(qm.df[,-which(grepl(pattern = "*PassQM", colnames(qm.df)))])

melt.bad.qm=reshape2::melt(bad.qm, id.vars="Site")

plot.melt.bad.qm=melt.bad.qm[-which(melt.bad.qm$value==0),]

ggplot(data = plot.melt.bad.qm, aes(x=variable, y=value, fill=variable))+
    geom_col(position = "dodge")+
    ggplot2::scale_fill_discrete(h = c(0, 240), l=65, c=100, name="QM")+
    facet_wrap(facets = ~Site)+
    ggplot2::theme(axis.title.x = element_blank())
