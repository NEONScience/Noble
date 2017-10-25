.grape.list<-function(list.test.dirs){
    for(l in 1:length(list.test.dirs)){
        test.files<-list.files(list.test.dirs[l])

        for(o in 1:length(test.files)){
            file.meta<-strsplit(test.files[o], split = "_")
            site<-unlist(file.meta[[1]][1])
            #print(site)
            date<-as.POSIXct(unlist(file.meta[[1]][2]), format ="%Y%m%d")-(3600*24)
            tempCSV<-read.csv(file=paste0(list.test.dirs[l], "/", test.files[o]))
            foundGrapes<-trimws(tempCSV[which(trimws(tempCSV[,2], "both")==trimws(tempCSV[,5], "both")),2], which = "both")
            foundGrapes<-data.frame(unique(foundGrapes))
            names(foundGrapes)<-"x"
            # siteDir<-paste0(dirGRAPETest, list.test.dirs[which(grepl(pattern = site, x=list.test.dirs))], "/")
            # grapeListFile <- list.files(siteDir)

            if(!file.exists(paste0(list.test.dirs[l], "/", site, "-Grapes.csv"))){
                write.csv(foundGrapes, file =paste0(list.test.dirs[l], "/", site, "-Grapes.csv"),  row.names = F)
            }
            if(file.exists(paste0(list.test.dirs[l], "/", site, "-Grapes.csv"))){
                existGrapes<- data.frame(read.csv(file =paste0(list.test.dirs[l], "/", site, "-Grapes.csv")))
                names(existGrapes)<-"x"
                foundGrapes<-data.frame(rbind(existGrapes, foundGrapes))
                foundGrapes<-data.frame(unique(foundGrapes$x))
                names(foundGrapes)<-"x"
                write.csv(foundGrapes, file =paste0(list.test.dirs[l], "/", site, "-Grapes.csv"),  row.names = F)
            }

        }
    }
}
