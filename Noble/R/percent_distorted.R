
# Private function for calculating the percent of wind measurements falling due to wind coming
# through the buffers on either side of the distorted flow field
#site="BLAN"
#save.dir="/Users/rlee/Dropbox/NEON/Commissioning/wind/D02-BLAN/"

###
.percent.distorted=function(site, bgn.month, end.month, save.dir){

    if(missing(save.dir)){save.dir=tempdir()}

    #get our data
    example.data=Noble::data.pull(site=site,
                                  dpID = "DP1.00001.001",
                                  bgn.month = bgn.month,
                                  end.month = end.month,
                                  time.agr = 30,
                                  package = 'expanded',
                                  save.dir = save.dir)

    example.data=Noble::un.ml.ize(example.data)

    DF.info=Noble::distorted.field(site=site)
    message("Site: ", site, ". DFF low: ", DF.info$distortedField[1], ", DFF high: ", DF.info$distortedField[2])
    if(DF.info$distortedField[1]>DF.info$distortedField[2]){
        DF1=data.frame(dir=example.data$windDirMean[which(data.table::between(example.data$windDirMean, lower =DF.info$distortedField[1], upper=360))])
        DF2=data.frame(dir=example.data$windDirMean[which(data.table::between(x = example.data$windDirMean, lower =0, upper=DF.info$distortedField[2]))])
        DF=rbind(DF1, DF2)
    }else{
        DF=data.frame(dir=example.data$windDirMean[which(data.table::between(example.data$windDirMean, lower =DF.info$distortedField[1], upper=DF.info$distortedField[2]))])
    }
    #UB_dir=example.data$windDirMean.000.010[which(data.table::between(example.data$windDirMean.000.010, lower = buff_hi[1], upper=buff_hi[2]))]

    DFF_plus_B=c(lower=(DF.info$distortedField[1]-10), upper=(DF.info$distortedField[2])+10)

    if(DFF_plus_B[1]>DFF_plus_B[2]){
        BDF1=data.frame(dir=example.data$windDirMean[which(data.table::between(example.data$windDirMean, lower =(DFF_plus_B[1]), upper=360))])
        BDF2=data.frame(dir=example.data$windDirMean[which(data.table::between(x = example.data$windDirMean, lower =0, upper=DFF_plus_B[2]))])
        BDF=rbind(DF1, DF2)
    }else{
        BDF=data.frame(dir=example.data$windDirMean[which(data.table::between(example.data$windDirMean, lower =DFF_plus_B[1], upper=DFF_plus_B[2]))])
    }
    All=(length(BDF[,1]))/length(example.data$windDirMean)*100
    DF.field.only=(length(DF[,1]))/length(example.data$windDirMean)*100
    out=c(All, DF.field.only)
    return(out)
}
