
# Private function for calculating the percent of wind measurements falling due to wind coming
# through the buffers on either side of the distorted flow field
#site="KONZ"
.percent.buffer=function(site, bgn.month, end.month, save.dir){
    ## Read in the site info and threshold info:
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

    # ATBD CALCS
    # Section 5.1
    ## Distorted Flow Test

    ###General parameters

     DF.info=Noble::distorted.field(site=site)
     message("Site: ", site, ". DFF low: ", DF.info$distortedField[1], ", DFF high: ", DF.info$distortedField[2])

     if(DF.info$distortedField[1]>DF.info$distortedField[2]){
         DF1=data.frame(dir=example.data$windDirMean[which(data.table::between(example.data$windDirMean, lower =DF.info$buff_low[1], upper=360))])
         DF2=data.frame(dir=example.data$windDirMean[which(data.table::between(x = example.data$windDirMean, lower =0, upper=DF.info$distortedField[2]))])
         DF=rbind(DF1, DF2)
     }else{
         DF=data.frame(dir=example.data$windDirMean[which(data.table::between(example.data$windDirMean, lower =DF.info$distortedField[1], upper=DF.info$distortedField[2]))])
     }
    LB_dir=example.data$windDirMean[which(data.table::between(example.data$windDirMean, lower = buff_low[1], upper=buff_low[2]))]
    UB_dir=example.data$windDirMean[which(data.table::between(example.data$windDirMean, lower = buff_hi[1], upper=buff_hi[2]))]

    pcnt.in.buffer=(length(UB_dir)+length(LB_dir))/length(example.data$windDirMean)*100
    message(paste0("Finshed with ", site))
    return(pcnt.in.buffer)
}
