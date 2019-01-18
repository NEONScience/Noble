site="CPER"
month="2017-12"
save.dir=tempdir()


internal.rad=function(site, month, save.dir, ...){
    library(ggplot2)
    if(!site %in% Noble::tis_site_config$SiteID[Noble::tis_site_config$Core.Relocatable=="Core"]){stop("Please select a core site!")}

    time.agr=30

    SPN1=Noble::data.pull(site=site, dpID = "DP1.00014.001", bgn.month = month, end.month = month, time.agr = time.agr, package = "basic", save.dir = save.dir)
    CMP22=Noble::data.pull(site=site, dpID = "DP1.00022.001", bgn.month = month, end.month = month, time.agr = time.agr, package = "basic", save.dir = save.dir)

    #merged=

    both=data.frame(Date= as.POSIXct(SPN1$startDateTime),
                    SPN1=SPN1[, grepl(x = colnames(SPN1), pattern = "gloRadMean*")],
                    CMP22 = CMP22[, grepl(x = colnames(CMP22), pattern = "shortRadMean*")])

    spear=cor.test(both$SPN1, both$CMP22, method = "spearman")

    plot=ggplot(data=both, aes(x=both$SPN1, y=both$CMP22))+
        geom_point(size=0.5)+
        xlab("SPN1 (Global Radiation)")+
        ylab("CMP22 (Shortwave Radiation)")+
        theme_light()+
        geom_abline(slope = 1, color='red')+
        coord_fixed()+
        ggtitle(label = paste0(site, ", ", month),
                subtitle = paste0("Spearman's rank correlation coefficient: ", signif(spear$estimate, digits=3)))
    ggsave(plot = plot, path = save.dir, filename = paste0("Int_rad_", site, "_", month, ".pdf"), device = "pdf", width = 7.5, height = 10, units = "in", dpi = 300)

    }
