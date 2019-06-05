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
#' Noble::tis.dq.test(site = site, bgn.month = bgn.month, end.month = end.month, save.dir=save.dir)


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
                                                           save.dir = save.dir,
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
                                                           save.dir = save.dir,
                                                           ml=x,
                                                           overwrite = overwrite)
    )
    # Perform Spearman's test, ML to ML comparison
    lapply(1:mls, function(i) try(RVAideMemoire::spearman.ci(var1 = picarro[[i]]$mean,var2 = licor[[i]]$mean, nrep = 1000, conf.level = .975)$conf.int[1])) %>%
        unlist() %>%
        `names<-`(value = paste("ML", 1:mls)) -> cors

    # if there were issues, coerce to NA
    cors[grepl(pattern = "^Error", x = cors)]=NA
    rho=cors

    rho.stats=data.frame(ml=names(rho), rho)

    write.csv(x=rho.stats, file = paste0(Noble:::.data.route(site, save.dir), "/picarro-licor.csv"), row.names = F)

    #if(pcnt.in.bound>90){validation="Pass"}else{validation="Fail"}
    if(all(is.na(rho))){system_compare="No Test"}else if(all(rho>0.5,na.rm = T)){system_compare="Pass"}else{system_compare="Fail"}
    result.string=data.frame(site=site,
                             bgn.month=bgn.month,
                             end.month=end.month,
                             #validation.check=validation, #removed for now
                             instrument.compare=system_compare,
                             external.compare="NA") #see Appendix of CTR for manual test instructions

    if(file.exists(Noble:::.result.route(save.dir))){
        dq.rpt <- data.frame(read.csv(file = Noble:::.result.route(save.dir), header = T, stringsAsFactors = T))
        dq.rpt <- rbind(dq.rpt, result.string)
        write.csv(x = dq.rpt, file = Noble:::.result.route(save.dir), row.names = F)
    }else{
        write.csv(x =result.string, file = Noble:::.result.route(save.dir), row.names = F)
    }
    rhdf5::h5closeAll()
}

