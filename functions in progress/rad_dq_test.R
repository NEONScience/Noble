# Radiation Data Quality testing
# Head ####
# outline the sites needed for testing
sitesNeeded<-c("CPER", "HEAL", "HARV", "JERC")


rad.pq.test<-function(site, bgn.month = "2017-02", end.month = "2017-04", dist.threshold, save.dir){
    require(data.table)
    require(dplyr)
    require(stringr)
    require(CommTis)
    require(solarPos)
    require(lubridate)
    require(tzinfo)
    require(metScanR)
    if(missing(dist.threshold)){
        message("No distance threshold set, defaulting to 5 km.")
        dist.threshold<-5
    }

  #  source('N:/Science/Science Commissioning Archive/SiteAndPayload/TisRadiationDataQuality/Common/grabUSCRN.R', echo=F)

    radDQDir<-"N:/Science/Science Commissioning Archive/SiteAndPayload/TisRadiationDataQuality/"
    # source('N:/Science/Science Commissioning Archive/SiteAndPayload/TisRadiationDataQuality/Common/grabUSCRN.R', echo=F)
    # source('C:/Users/rlee/Dropbox/GitHub/Commissioning-TIS-rhlee12/Common/pack/CommTis/R/NEON_API.R', echo=F)
    siteMetaDF<-read.csv(paste0(radDQDir, "Common/siteSummary.csv"), header = T)
    siteConfig<-read.csv("N:/Science/Science Commissioning Archive/SiteAndPayload/TIS_site_config.csv", header=T)
    TimeBgn<-as.POSIXct("2016-05-01", format="%Y-%m-%d", tz="UTC")
    TimeEnd<-as.POSIXct("2016-06-30 00:00:01", format="%Y-%m-%d %H:%M:%S", tz="UTC")
    resultsDF<-data.frame("Site"=site,
                          "intVar_Ftest"=rep("", length(site)),
                          "int_parResult"=rep("", length(site)),
                          "int_gddResult"=rep("", length(site)),
                          "int_gdd_ratio_A"=rep("", length(sitesNeeded)),
                          "int_gdd_ratio_B"=rep("", length(sitesNeeded)),
                          "ext_Spearman"=rep("", length(sitesNeeded)),
                          "ext_Spearman_Result"=rep("", length(sitesNeeded))
    )

    DateBgn <- round(TimeBgn, "days")
    DateEnd <- round(TimeEnd, "days")
    TimeAgr <- 30
    Pack <- "basic"
    Kpi <- "Radiation"
    useSpatial <- F

    # Get the data ####
    # for (s in 1:length(sitesNeeded)){
    #     prod <- nneo::nneo_products()
    #     prodName <- prod[which(prod$productCategory == "Level 1 Data Product" & prod$productScienceTeamAbbr =="TIS"), c("productName","productCodeLong")]
    #     prodKpi <<- prodName[grep(pattern = Kpi, prodName$productName, ignore.case = T),]
    #
    #     domn <- siteMetaDF$Domain[which(siteMetaDF$Site==sitesNeeded[s])]
    #     domn<- str_pad(domn, width = 2, side = "left", pad = "0")
    #     dpIDs<-CommTis::site.cfig(Site=sitesNeeded[s], Kpi="Radiation")$productCodeLong
    #     dpIDs<-gsub(pattern = "NEON.DOM.SITE.", replacement = "", x = dpIDs)
    #     dataPrd<-CommTis::site.cfig(Site=sitesNeeded[s], Kpi="Radiation")$productName
    #     filePath <- paste0(radDQDir, "D", domn, "-", sitesNeeded[s])
    #     for(i in 1:length(dpIDs)){
    #         dataFile<- paste0("NEON.D", domn,".", sitesNeeded[s],".", dpIDs[i], "_REQ_", DateBgn, "_", DateEnd, "_", TimeAgr,"min_", Pack, ".csv.gz")
    #         fullPath <- paste(filePath, dataFile, sep="/")
    #         if (!file.exists(fullPath)){
    #             print(paste("Currently downloading", dpIDs[i], "data for:", sitesNeeded[s]))
    #             # Get the requested data
    #             grabNEON(Site=sitesNeeded[s], time_bgn = TimeBgn, time_end=TimeEnd , data_var = dataPrd[i],
    #                      time_agr = TimeAgr, Pack = Pack, dat_dir = filePath)
    #
    #         }
    #     }
    # }

    # Variance Testing ####
    for (s in 1:length(sitesNeeded)){
        varNames<-paste0(unlist(listKpi), "Variance")
        varNames<-varNames[-which(grepl(pattern = "LW", x= varNames))]
        domn <- siteMetaDF$Domain[which(siteMetaDF$Site==sitesNeeded[s])]
        domn<- str_pad(domn, width = 2, side = "left", pad = "0")
        filePath <- paste0(radDQDir, "D", domn, "-", sitesNeeded[s])
        availFiles<-list.files(filePath)
        temp1<-grep(availFiles, pattern = DateBgn)
        temp2<-grep(availFiles, pattern = DateEnd)
        currFilesIndex<-as.numeric(intersect(temp1, temp2))
        neededFiles<-availFiles[currFilesIndex]

        tempData <- data.frame(read.csv(paste0(filePath,"/", neededFiles[1])))
        varIndex <- c(1, which(gsub(colnames(tempData), pattern = "[0-9]||[.]", replacement = "") %in% varNames))
        varData<-tempData[,varIndex]
        if(length(neededFiles)>1){
            for(i in 2:length(neededFiles)){
                tempData<-read.csv(paste0(filePath,"/", neededFiles[i]))
                varIndex <- c(1, which(gsub(colnames(tempData), pattern = "[0-9]||[.]", replacement = "") %in% varNames))
                tempVarData<-tempData[,varIndex]
                #colnames(tempVarData)<-c(1, paste(colnames(temp[2:length(temp)]), substr(neededFiles[i], 19, 24), sep = "."))
                varData<-merge(varData, tempVarData)
            }}

        # make an index of data columns
        # varNames<-paste0(unlist(listKpi), "Variance")
        # varIndex <- c(1)
        # for(v in 1:length(varNames)){
        #     varIndex<-append(varIndex, grep(pattern = varNames[v], colnames(allData)))
        # }
        # # get only data columns
        # meanData<-allData[,varIndex]

        # convert measuremnt times to local time
        timeZone <- siteConfig$Time.Zone[which(siteConfig$SiteID==sitesNeeded[s])]
        varData$POSIXseq<-as.POSIXct(varData$POSIXseq, tz="UTC")
        varData$POSIXseq<-as.POSIXct(format(varData$POSIXseq, tz=timeZone, usetz = T), tz=timeZone, usetz = T)

        # subset to first and last week of the commissioning period
        frstWeekData<- varData[1:(7*48),]
        lastWeekData<- varData[(length(varData$POSIXseq)-(7*48)+1):(length(varData$POSIXseq)),]

        testTime <- c("00:00:00", "00:30:00", "01:00:00", "01:30:00", "02:00:00", "02:30:00", "03:00:00",
                      "03:30:00", "04:00:00")

        frstWeekDataNight<-frstWeekData[which(strftime(frstWeekData$POSIXseq, format="%H:%M:%S", tz=timeZone) %in% testTime),]

        lastWeekDataNight<-lastWeekData[which(strftime(lastWeekData$POSIXseq, format="%H:%M:%S", tz=timeZone) %in% testTime),]
        print(paste0(sitesNeeded[s], " F-tests:"))
        fTest<-c()
        for(i in i:length(colnames(frstWeekDataNight))){
            if(!all(is.na(frstWeekDataNight[,i]))&!all(is.na(lastWeekDataNight[,i]))){
                x<-frstWeekDataNight[,i]
                y<-lastWeekDataNight[,i]
                singlefTest<-var.test(x,y, conf.level = 0.99)
                print(paste0(colnames(frstWeekDataNight)[i], ": ", round(singlefTest$estimate, digits = 3)))
                print(paste0(round(mean(x, na.rm = T), digits=4), "(", round(sd(x, na.rm = T), digits=4),"):",round(mean(y, na.rm = T), digits = 4),"(", round(sd(y, na.rm = T), digits=4),")"))
                fTest<-append(fTest, var.test(x,y, conf.level = 0.99))
                print("")
            }
        }

        print("")
        resultsDF$intVar_Ftest[which(resultsDF$Site==sitesNeeded[s])]<-round(fTest$estimate, digits = 3)
    }



    ################## A little plotting investigation #################
    # for(i in 2:length(colnames(varData))){
    #     varData$POSIXseq<-as.POSIXct(varData$POSIXseq)
    #         if(!all(is.na(varData[,i])))
    #         {
    #     plot(x=varData$POSIXseq, y=varData[,i], xlab = "Date", ylab = colnames(varData)[i])
    #             }
    # }



    # Perform internal consistency on PAR sensors (tower versus soil plot)

    for (s in 1:length(sitesNeeded)){

        if (siteMetaDF$classification[which(siteMetaDF$Site==sitesNeeded[s])]=="grassland"){
            rhoThreshold <- 0.9
        }else{rhoThreshold <- 0.65
        }

        varNames<-c("linePARMean", "PARMean")
        domn <- siteMetaDF$Domain[which(siteMetaDF$Site==sitesNeeded[s])]
        domn<- str_pad(domn, width = 2, side = "left", pad = "0")
        filePath <- paste0(radDQDir, "D", domn, "-", sitesNeeded[s])
        availFiles<-list.files(filePath)
        availFiles<-availFiles[which(grepl(availFiles, pattern = ".00024.|.00066."))]

        temp1<-grep(availFiles, pattern = DateBgn)
        temp2<-grep(availFiles, pattern = DateEnd)
        currFilesIndex<-as.numeric(intersect(temp1, temp2))
        neededFiles<-availFiles[currFilesIndex]

        tempData <- data.frame(read.csv(paste0(filePath,"/", neededFiles[1])))
        varIndex <- c(1, which(gsub(colnames(tempData), pattern = "[0-9]||[.]", replacement = "") %in% varNames))
        parData<-tempData[,varIndex]
        if(length(neededFiles)>1){
            for(i in 2:length(neededFiles)){
                tempData<-read.csv(paste0(filePath,"/", neededFiles[i]))
                parIndex <- c(1, which(gsub(colnames(tempData), pattern = "[0-9]||[.]", replacement = "") %in% varNames))
                tempParData<-tempData[,parIndex]
                #colnames(tempVarData)<-c(1, paste(colnames(temp[2:length(temp)]), substr(neededFiles[i], 19, 24), sep = "."))
                parData<-merge(parData, tempParData)
            }}

        towerPARIndex<- grep(colnames(parData), pattern = "^PARMean")
        soilPlotPARIndex<- grep(colnames(parData), pattern = "linePARMean")

        siteResults<-c()
        j<-2
        for (j in 2:length(towerPARIndex)){

            tempDF<-as.data.frame(cbind("x"=parData[,towerPARIndex[j-1]],  "y"=parData[,towerPARIndex[j]]))
            testDF<- tempDF[which(tempDF$x>=10&tempDF$y>=10),]

            spearmanTest<-cor.test(testDF$x, testDF$y, method = "spearman", conf.level = 0.95, na.rm = T)

            if(spearmanTest$estimate>=rhoThreshold){
                parCompResult<-"Pass"}else{parCompResult<-"Fail"}

            print(paste0(sitesNeeded[s], " tower ML ", j-1, "-", j, " rho value: ", round(spearmanTest$estimate, digits =2), ". Site is a ", siteMetaDF$classification[which(siteMetaDF$Site==sitesNeeded[s])], " site. ", parCompResult, "." ))
            siteResults<-append(siteResults, parCompResult)
        }

        for (j in 2:length(soilPlotPARIndex)){

            tempDF<-as.data.frame(cbind("x"=parData[,soilPlotPARIndex[j-1]],  "y"=parData[,soilPlotPARIndex[j]]))
            testDF<- tempDF[which(tempDF$x>=10&tempDF$y>=10),]

            spearmanTest<-cor.test(testDF$x, testDF$y, method = "spearman", conf.level = 0.95, na.rm = T)

            if(spearmanTest$estimate>=rhoThreshold){
                parCompResult<-"Pass"}else{parCompResult<-"Fail"}

            print(paste0(sitesNeeded[s], " soil plots ", j-1, "-", j, " rho value: ", round(spearmanTest$estimate, digits =2), ". Site is a ", siteMetaDF$classification[which(siteMetaDF$Site==sitesNeeded[s])], " site. ", parCompResult, "." ))
            siteResults<-append(siteResults, parCompResult)
        }
        numFails <- sum(grep(siteResults, pattern = "Fail"))
        if(rhoThreshold==0.9&&numFails==0){int_parResult<-"Pass"}
        if(rhoThreshold==0.65&&numFails<=1){int_parResult<-"Pass"}else{int_parResult<-"Pass"}
        resultsDF$int_parResult[which(resultsDF$Site==sitesNeeded[s])]<-int_parResult
    }

    # Internal consistency for all radiometers on tower top
    for (s in 1:length(sitesNeeded)){
        topML<- siteConfig$Num.of.MLs[which(siteConfig$SiteID==sitesNeeded[s])]

        varNames<-c("inSWMean", "gloRadMean", "shortRadMean")
        varNames<-paste0(varNames, ".000.0", topML, "0")
        domn <- siteMetaDF$Domain[which(siteMetaDF$Site==sitesNeeded[s])]
        domn<- str_pad(domn, width = 2, side = "left", pad = "0")
        filePath <- paste0(radDQDir, "D", domn, "-", sitesNeeded[s])
        availFiles<-list.files(filePath)
        availFiles<-availFiles[which(grepl(availFiles, pattern = ".00023.|.00022.|.00014."))]

        temp1<-grep(availFiles, pattern = DateBgn)
        temp2<-grep(availFiles, pattern = DateEnd)
        currFilesIndex<-as.numeric(intersect(temp1, temp2))
        neededFiles<-availFiles[currFilesIndex]

        tempData <- data.frame(read.csv(paste0(filePath,"/", neededFiles[1])))
        varIndex <- c(1, which(colnames(tempData)  %in% varNames))
        radiometerData<-tempData[,varIndex]
        if(length(neededFiles)>1){
            for(i in 2:length(neededFiles)){
                tempData<-read.csv(paste0(filePath,"/", neededFiles[i]))
                varIndex <- c(1, which(colnames(tempData) %in% varNames))
                tempRMeterData<-tempData[,varIndex]
                radiometerData<-merge(radiometerData, tempRMeterData)
            }}
        radiometerSiteResults<-c()
        j<-3

        for (j in 3:length(colnames(radiometerData))){
            testDF<-as.data.frame(cbind("x"=radiometerData[,j-1],  "y"=radiometerData[,j]))

            spearmanTest<-cor.test(testDF$x, testDF$y, method = "spearman", conf.level = 0.95, na.rm = T)

            if(spearmanTest$estimate>=0.9){
                rMeterCompResult<-"Pass"}else{rMeterCompResult<-"Fail"}

            print(paste0(sitesNeeded[s], " radiometer pair ", j-2, "-", j-1, " rho value: ", round(spearmanTest$estimate, digits =2), ". ", rMeterCompResult, "." ))
            siteResults<-append(siteResults, parCompResult)
        }
        if(length(colnames(radiometerData))==4){
            testDF<-as.data.frame(cbind("x"=radiometerData[,2],  "y"=radiometerData[,4]))

            spearmanTest<-cor.test(testDF$x, testDF$y, method = "spearman", conf.level = 0.95, na.rm = T)

            if(spearmanTest$estimate>=0.9){
                rMeterCompResult<-"Pass"}else{rMeterCompResult<-"Fail"}

            print(paste0(sitesNeeded[s], " radiometer pair ",1, "-", 3, " rho value: ", round(spearmanTest$estimate, digits =2), ". ", rMeterCompResult, "." ))
            siteResults<-append(siteResults, parCompResult)
        }
    }


    # Internal consistency for direct/diffuse radiation ####
    for (s in 1:length(sitesNeeded)){
        print(sitesNeeded[s])
        varNames<-c("gloRadMean", "dirRadMean", "difRadMean")
        domn <- siteMetaDF$Domain[which(siteMetaDF$Site==sitesNeeded[s])]
        domn<- str_pad(domn, width = 2, side = "left", pad = "0")
        filePath <- paste0(radDQDir, "D", domn, "-", sitesNeeded[s])
        availFiles<-list.files(filePath)
        availFiles<-availFiles[which(grepl(availFiles, pattern = ".00014."))]

        temp1<-grep(availFiles, pattern = DateBgn)
        temp2<-grep(availFiles, pattern = DateEnd)
        currFilesIndex<-as.numeric(intersect(temp1, temp2))
        neededFiles<-availFiles[currFilesIndex]

        tempData <- data.frame(read.csv(paste0(filePath,"/", neededFiles[1])))
        varIndex <- c(1, which(gsub(colnames(tempData), pattern = "[0-9]||[.]", replacement = "") %in% varNames))
        gddData<-tempData[,varIndex]

        timeZone <- siteConfig$Time.Zone[which(siteConfig$SiteID==sitesNeeded[s])]
        gddData$POSIXseq<-as.POSIXct(gddData$POSIXseq, tz="UTC")
        gddData$POSIXseq<-as.POSIXct(format(gddData$POSIXseq, tz=timeZone, usetz = T), tz=timeZone, usetz = T)

        colnames(gddData)<-gsub(x=colnames(gddData), pattern = "[[:digit:]]|[\\.]", "")

        gddData<-cbind(gddData, "julianDay"=julianDay(year(gddData$POSIXseq), month(gddData$POSIXseq), day(gddData$POSIXseq), hour(gddData$POSIXseq), minute(gddData$POSIXseq), second(gddData$POSIXseq), tz=siteMetaDF$UTC_offset[which(siteMetaDF$Site==sitesNeeded[s])]))
        gddData<-cbind(gddData, solarPosition(jd=gddData$julianDay, lon=siteMetaDF$longitude[which(siteMetaDF$Site==sitesNeeded[s])], lat = siteMetaDF$latitude[which(siteMetaDF$Site==sitesNeeded[s])]))
        gddData<-cbind(gddData, "dirDifSum"=(gddData$difRadMean+gddData$dirRadMean))

        highVal<-gddData[which(gddData$dirDifSum>=50),]

        pop1<-highVal[which(between(highVal$zenith, 0, 75)),]
        pop2<-highVal[which(between(highVal$zenith, 75, 93)),]


        pop1<-cbind(pop1, "ratio"=(pop1$gloRadMean/pop1$dirDifSum))
        pop2<-cbind(pop2, "ratio"=(pop2$gloRadMean/pop2$dirDifSum))
        # ratio1<-mean(sum(pop1$gloRadMean))/mean(sum(pop1$dirDifSum)) #plus or minus 8% ratio tolerance
        # ratio2<-mean(sum(pop2$gloRadMean))/mean(sum(pop2$dirDifSum)) #plus or minus 15% ratio tolerance
        #

        ratio1<-mean(pop1$ratio)
        ratio2<-mean(pop2$ratio)
        if (between(ratio1, 0.92, 1.08)&between(ratio2, 0.85, 1.15)){
            gddCompResults<-"Pass"
        }else{
            gddCompResults<-"Fail"
        }
        print(gddCompResults)
        print(ratio1)

        resultsDF$int_gddResult[which(resultsDF$Site==sitesNeeded[s])]<-gddCompResults
        resultsDF$int_gdd_ratio_A[which(resultsDF$Site==sitesNeeded[s])]<-round(ratio1, digits = 2)
        resultsDF$int_gdd_ratio_B[which(resultsDF$Site==sitesNeeded[s])]<-round(ratio2, digits = 2)
    }


    ##julianDay()


    # Perform external GloRad comparison test on sites ####

    for (s in 1:length(sitesNeeded)){

        domn <- siteMetaDF$Domain[which(siteMetaDF$Site==sitesNeeded[s])]
        domn<- str_pad(domn, width = 2, side = "left", pad = "0")
        filePath <- paste0(radDQDir, "D", domn, "-", sitesNeeded[s])
        availFiles<-list.files(filePath)
        temp2<-grep(availFiles, pattern = DateEnd)
        temp3<-grep(availFiles, pattern = "DP1.00014.001")
        currFilesIndex<-as.numeric(intersect(temp2, temp3))
        neededFile<-availFiles[currFilesIndex]


        currSiteGloRad<-read.csv(file=paste0(filePath, "/", neededFile), header=T)


        if (is.na(siteMetaDF$nearestUSCRN[which(siteMetaDF$Site==sitesNeeded[s])])){
            resultsDF$ext_Spearman[which(resultsDF$Site==sitesNeeded[s])]<-"No nearby radiation sensors"
            resultsDF$ext_Spearman_Result[which(resultsDF$Site==sitesNeeded[s])]<-"NA"
        }else
        {extSite<-grabUSCRN(stationID = siteMetaDF$nearestUSCRN[which(siteMetaDF$Site==sitesNeeded[s])], timeScale = "subhourly", TimeBgn = TimeBgn, TimeEnd = TimeEnd)

        # Convert external data to to 30 minute averaged data
        extRad<-data.frame(extSite %>%
                               group_by(UTC_DATE = cut(UTC_DATE, breaks="30 min")) %>%
                               summarize(SOLAR_RADIATION = mean(SOLAR_RADIATION)))

        bothRad<-data.frame(cbind(extRad, currSiteGloRad[,which(grepl(x = colnames(currSiteGloRad), pattern = "gloRadMean"))]))
        colnames(bothRad)<-c("UTC_Date", "ExtRad", "NEONRad")

        spearmanTest<-cor.test(bothRad$ExtRad, bothRad$NEONRad, method = "spearman", conf.level = 0.95)

        if(spearmanTest$estimate>=0.90){
            testResult<-"Pass"}else{testResult<-"Fail"}
        resultsDF$ext_Spearman[which(resultsDF$Site==sitesNeeded[s])]<-spearmanTest$estimate
        resultsDF$ext_Spearman_Result[which(resultsDF$Site==sitesNeeded[s])]<-testResult}

    }


    # # all radiation
    # for (s in 1:length(sitesNeeded)){
    #     domn <- siteMetaDF$Domain[which(siteMetaDF$Site==sitesNeeded[s])]
    #     filePath <- paste0(radDQDir, "D", domn, "-", sitesNeeded[s])
    #     availFiles<-list.files(filePath)
    #     temp1<-grep(availFiles, pattern = DateBgn)
    #     temp2<-grep(availFiles, pattern = DateEnd)
    #     currFilesIndex<-as.numeric(intersect(temp1, temp2))
    #     neededFiles<-availFiles[currFilesIndex]
    #
    #     allData<-data.frame(read.csv(paste0(filePath,"/", neededFiles[1])))
    #     if(length(neededFiles)>1){
    #     for(i in 2:length(neededFiles)){
    #         temp<-read.csv(paste0(filePath,"/", neededFiles[i]))
    #         colnames(temp)<-c(1, paste(colnames(temp[2:length(temp)]), substr(neededFiles[i], 19, 24), sep = "."))
    #         allData<-merge(allData, temp)
    #     }}
    #
    #     # make an index of data columns
    #     varNames<-paste0(unlist(listKpi), "Variance")
    #     varIndex <- c(1)
    #     for(v in 1:length(varNames)){
    #         varIndex<-append(varIndex, grep(pattern = varNames[v], colnames(allData)))
    #     }
    #     # get only data columns
    #     meanData<-allData[,varIndex]
    #
    #     # convert measuremnt times to local time
    #     timeZone <- siteConfig$Time.Zone[which(siteConfig$SiteID==sitesNeeded[s])]
    #     meanData$POSIXseq<-as.POSIXct(meanData$POSIXseq, tz="UTC")
    #     meanData$POSIXseq<-as.POSIXct(format(meanData$POSIXseq, tz=timeZone, usetz = T), tz=timeZone, usetz = T)
    #
    #     # subset to first and last week of the commissioning period
    #     frstWeekData<- meanData[1:(7*48),]
    #     lastWeekData<- meanData[(length(meanData$POSIXseq)-(7*48)+1):(length(meanData$POSIXseq)),]
    #
    #     testTime <- c("00:00:00", "00:30:00", "01:00:00", "01:30:00", "02:00:00", "02:30:00", "03:00:00",
    #                   "03:30:00", "04:00:00")
    #
    #     frstWeekDataNight<-frstWeekData[which(strftime(frstWeekData$POSIXseq, format="%H:%M:%S", tz=timeZone) %in% testTime),]
    #     lastWeekDataNight<-lastWeekData[which(strftime(lastWeekData$POSIXseq, format="%H:%M:%S", tz=timeZone) %in% testTime),]
    #     x<-as.matrix(frstWeekDataNight[,2:length(frstWeekDataNight)])
    #     y<- as.matrix(lastWeekDataNight[,2:length(lastWeekDataNight)])
    #     fTest<-var.test(x,y, conf.level = 0.95)
    #     resultsDF$intVar_Ftest[which(resultsDF$Site==sitesNeeded[s])]<-fTest$estimate
    #
    # }
    #
}
