## Funciton start
date.extract=function(data, bgn.date, end.date){
    data.out=data[which(as.POSIXct(data[,1])>bgn.date & as.POSIXct(data[,1])<end.date),]
    return(data.out)
}
