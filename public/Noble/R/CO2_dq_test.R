############################################################################################
#' @title  Perform A Commissioning DQ Test on CO2 Concentration Data

#'#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description For the specified dates, site, variables, and data product or name of family of data
#' products, data are downloaded and saved to the specifed directory. Process quality calculations are
#'  then performed and written to a results file in save.dir.
#'
#' @param \code{site} Parameter of class character. The NEON site data should be downloaded for.
#' @param \code{bgn.month} Parameter of class character. The year-month (e.g. "2017-01") of the first
#'  month to get data for.
#' @param \code{end.month} Parameter of class character. The year-month (e.g. "2017-01") of the last
#'  month to get data for.
#' @param \code{save.dir} Parameter of class character. The local directory where data files should
#' be saved.
#'
#' @return Writes data files to the specified directory.

#' @keywords process quality, data quality, gaps, commissioning

#' @examples
#' site = "BART"
#' bgn.month = "2017-09"
#' end.month = "2017-10"
#' save.dir<-"~/Desktop/"
#' Noble:::tis.dq.test(site = site, bgn.month = bgn.month, end.month = end.month, save.dir=save.dir)


#' @seealso Currently none
#' @export
#'
# changelog and author contributions / copyrights
#   Robert Lee (2018-03-25)
#     Original creation
#
##############################################################################################

co2.dq.test=function(site = site,  bgn.month, end.month, save.dir, overwrite=F){
    library(magrittr)
    options(stringsAsFactors = FALSE)

    # Validation checking
    time.agr = 30
    files=Noble::pull.eddy.data(site = site, bgn.month = bgn.month, end.month = end.month, package="expanded", save.dir=save.dir)
   # file.dir=Noble:::.data.route(site = site, save.dir = save.dir)


    #all.data=lapply(files, function(x) rhdf5::h5read(file = paste0(file.dir, "/", x), name = site))
    #names(all.data)=c(bgn.month, end.month)
# Validation Checking -- This Code tests STORAGE Exchange, not the Turbulent Exchange (as required).
# Updates needed once TE has published validation measurements.
    # high=data.frame(cbind(
    #     meas=as.numeric(na.exclude(c(all.data[[1]]$dp01$data$co2Stor$co2High_30m$rtioMoleDryCo2$mean,
    #                                  all.data[[2]]$dp01$data$co2Stor$co2High_30m$rtioMoleDryCo2$mean))),
    #     ref=as.numeric(na.exclude(c(all.data[[1]]$dp01$data$co2Stor$co2High_30m$rtioMoleDryCo2Refe$mean,
    #                                 all.data[[2]]$dp01$data$co2Stor$co2High_30m$rtioMoleDryCo2Refe$mean)))
    # ))
    #
    # high$dev=abs(high$meas-high$ref)
    # high$pcnt=high$dev/high$ref
    #
    # med=data.frame(cbind(
    #     meas=as.numeric(na.exclude(c(all.data[[1]]$dp01$data$co2Stor$co2Med_30m$rtioMoleDryCo2$mean,
    #                                  all.data[[2]]$dp01$data$co2Stor$co2Med_30m$rtioMoleDryCo2$mean))),
    #     ref=as.numeric(na.exclude(c(all.data[[1]]$dp01$data$co2Stor$co2Med_30m$rtioMoleDryCo2Refe$mean,
    #                                 all.data[[2]]$dp01$data$co2Stor$co2Med_30m$rtioMoleDryCo2Refe$mean)))
    # ))
    #
    # med$dev=abs(med$meas-med$ref)
    # med$pcnt=med$dev/med$ref
    #
    # low=data.frame(cbind(
    #     meas=as.numeric(na.exclude(c(all.data[[1]]$dp01$data$co2Stor$co2Low_30m$rtioMoleDryCo2$mean,
    #                                  all.data[[2]]$dp01$data$co2Stor$co2Low_30m$rtioMoleDryCo2$mean))),
    #     ref=as.numeric(na.exclude(c(all.data[[1]]$dp01$data$co2Stor$co2Low_30m$rtioMoleDryCo2Refe$mean,
    #                                 all.data[[2]]$dp01$data$co2Stor$co2Low_30m$rtioMoleDryCo2Refe$mean)))
    # ))
    #
    # low$dev=abs(low$meas-low$ref)
    # low$pcnt=low$dev/low$ref
    #
    # pcnt.in.bound=((length(high$pcnt[high$pcnt<0.015])+
    #                     length(med$pcnt[med$pcnt<0.015])+
    #                     length(low$pcnt[low$pcnt<0.015]))/
    #                    (length(high$pcnt)+length(med$pcnt)+length(low$pcnt)))*100
    #
    # write.csv(x=data.frame(pcnt.in.bound), file = paste0(Noble:::.data.route(site, save.dir), "/validation.csv"), row.names = F)

    # Cross check LICOR v Picarro
    mls=Noble::tis_site_config$num.of.mls[Noble::tis_site_config$site.id==site]
    picarro=lapply(seq(mls), function(x) Noble::hdf5.to.df(site=site,
                                                           files = files,
                                                           data.type = "data",
                                                           var.name = "rtioMoleDryCo2",
                                                           meas.name="isoCo2",
                                                           bgn.month = bgn.month,
                                                           end.month = end.month,
                                                           time.agr = time.agr,
                                                           save.dir = Noble:::.data.route(site=site, save.dir=save.dir),
                                                           ml=x,
                                                           overwrite = overwrite)
    )
    licor=lapply(seq(mls), function(x) Noble::hdf5.to.df(site=site,
                                                           files = files,
                                                           data.type = "data",
                                                           var.name = "rtioMoleDryCo2",
                                                           meas.name="co2Stor",
                                                           bgn.month = bgn.month,
                                                           end.month = end.month,
                                                           time.agr = time.agr,
                                                           save.dir = Noble:::.data.route(site=site, save.dir=save.dir),
                                                           ml=x,
                                                           overwrite = overwrite)
    )

    ## Turb ex. validation testing
    tower.top=Noble::tis_site_config$num.of.mls[Noble::tis_site_config$site.id==site]
    turb.validation.list=lapply(files, function(file) rhdf5::h5read(file, paste0(site, "/dp01/data/co2Turb/000_0", tower.top, "0_01m/rtioMoleDryCo2Vali")))
    keep.turb=lapply(turb.validation.list, class)=="data.frame"
    turb.validation.list=turb.validation.list[keep.turb]

    turb.validation.df=do.call(rbind, turb.validation.list)
    turb.validation.df=turb.validation.df[!is.nan(turb.validation.df$mean),]

    turb.validation.df.real=turb.validation.df[turb.validation.df$rtioMoleDryCo2Refe>0,]
    turb.validation.df.zero=turb.validation.df[turb.validation.df$rtioMoleDryCo2Refe<1,]

    write.csv(x = turb.validation.df, file = paste0(Noble:::.data.route(site=site, save.dir = save.dir), site, "_co2TurbVali_", bgn.month, "-", end.month, ".csv"), row.names = F)

    pcnt.error=100*abs(turb.validation.df.real$mean-turb.validation.df.real$rtioMoleDryCo2Refe)/turb.validation.df.real$rtioMoleDryCo2Refe

    if(all(pcnt.error<5)){validation="Pass"}else{validation="Fail"}
#browser()
    # Perform Spearman's test, ML to ML comparison
    lapply(1:mls, function(i) try(RVAideMemoire::spearman.ci(var1 = picarro[[i]]$mean,var2 = licor[[i]]$mean, nrep = 1000, conf.level = .975)$conf.int[1])) %>%
        unlist() %>%
        `names<-`(value = paste("ML", 1:mls)) -> cors

    # if there were issues, coerce to NA
    cors[grepl(pattern = "^Error", x = cors)]=NA
    rho=cors

    rho.stats=data.frame(ml=names(rho), rho)

    write.csv(x=rho.stats, file = paste0(Noble:::.data.route(site, save.dir), "/picarro-licor.csv"), row.names = F)


    # perform site to site comparison
    # UNDE is the only comparable site so far
    external.compare="NA"
    if(site=="UNDE"){
        Syv=read.csv("/Volumes/neon/Science/Science Commissioning Archive/SiteAndPayload/TisCO2ConcentrationDataQuality/D05-UNDE/AMF_US-Syv_BASE_HH_10-5.csv", skip = 2)

        # Read in and combine the D05-UNDE Data
        UNDE=Noble::hdf5.to.df(site = "UNDE", files = files, data.type = 'data', meas.name = "co2Turb", var.name = "rtioMoleDryCo2Cor", time.agr = 30, bgn.month = "2017-09", end.month = "2017-10", save.dir = "/Volumes/neon/Science/Science Commissioning Archive/SiteAndPayload/TisCO2ConcentrationDataQuality/D05-UNDE/")
        # UNDE=rbind(
        #     read.csv("/Volumes/neon/Science/Science Commissioning Archive/SiteAndPayload/TisCO2ConcentrationDataQuality/D05-UNDE/co2Turb_UNDE_2017-09-01-2017-09-30.csv"),
        #     read.csv("/Volumes/neon/Science/Science Commissioning Archive/SiteAndPayload/TisCO2ConcentrationDataQuality/D05-UNDE/co2Turb_UNDE_2017-10-01-2017-10-31.csv")
        # )

        # Convert US-Syv timestamps to POSIX format
        Syv$TIMESTAMP_START=as.POSIXct(as.character(Syv$TIMESTAMP_START), format="%Y%m%d%H%M", tz="UTC")

        # Convert the D05-UNDE timestamps to POSIX format
        UNDE$timeBgn=as.POSIXct(UNDE$startDateTime, format="%Y-%m-%d %H:%M:%S ", tz="UTC")-lubridate::hours(6) #Ameriflux users LST for times, so offset is 6 From NEON

        # Subset the US-Syv to the same dates as the D05-UNDE
        sub_syv=Syv[(Syv$TIMESTAMP_START<as.POSIXct("2017-11-01")&Syv$TIMESTAMP_START>=as.POSIXct("2017-09-01")),]

        # replace -9999 values with NA
        sub_syv[sub_syv==-9999]=NA

        plot.data=merge(x=UNDE, y=sub_syv, by.x="timeBgn", by.y="TIMESTAMP_START")
        plot.data=data.frame(Date=as.POSIXct(plot.data$startDateTime), UNDE=plot.data$mean, USSyv=plot.data$CO2_1_1_1)
        cor.data=plot.data

        # plot.data=reshape2::melt(plot.data, id.vars="Date")
        # p=ggplot2::ggplot(data=plot.data, ggplot2::aes(x=Date, y=value, color=as.factor(variable)))+
        #     ggplot2::geom_path()+
        #     ggplot2::theme_bw()+
        #     ggplot2::ylab("Measured CO2 concentration (ppm)")+
        #     ggplot2::labs(color="Measurement Location", title="CO2 Concentrations at D05-UNDE and colocated AmeriFlux US-Syv")+
        #     ggplot2::scale_color_manual(values = c("red", "blue"))
        rho=cor.test(x=cor.data$UNDE, y = cor.data$USSyv, conf.level = 0.975)
        if(rho>0.5){external.compare="Pass"}else{external.compare="Fail"}
    }

    #if(pcnt.in.bound>90){validation="Pass"}else{validation="Fail"}
    if(all(is.na(rho))){system_compare="No Test"}else if(all(rho>0.5,na.rm = T)){system_compare="Pass"}else{system_compare="Fail"}
    result.string=data.frame(site=site,
                             bgn.month=bgn.month,
                             end.month=end.month,
                             validation.check=validation, #removed for now
                             instrument.compare=system_compare,
                             external.compare=external.compare) #see Appendix of CTR for manual test instructions

    if(file.exists(Noble:::.result.route(save.dir))){
        dq.rpt <- data.frame(read.csv(file = Noble:::.result.route(save.dir), header = T, stringsAsFactors = T))
        dq.rpt <- rbind(dq.rpt, result.string)
        write.csv(x = dq.rpt, file = Noble:::.result.route(save.dir), row.names = F)
    }else{
        write.csv(x =result.string, file = Noble:::.result.route(save.dir), row.names = F)
    }
    rhdf5::h5closeAll()
}

