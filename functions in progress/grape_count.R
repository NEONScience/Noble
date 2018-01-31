#TEST BLOCK

.grape.count<-function(grape.df){

    grape.range.df=data.frame(mac=NA, min=NA, max=NA)
    for(i in 1:length(grape.df$Grape.MAC.Address)){
        mac=grape.df$Grape.MAC.Address[i]
        temp=colnames(grape.df[i,which(!is.na(grape.df[i,]))])

        temp=temp[-1]


        min=min(temp)
        max=max(temp)
        new.row=c(mac, min, max)
        grape.range.df = rbind(grape.range.df, new.row)
        rm(temp, new.row)
    }

    grape.range.df=grape.range.df[-1,]
    while.loop.df=grape.range.df
    total.range = data.frame(min=min(colnames(grape.df[,-1])), max=max(colnames(grape.df[,-1])))

    paired.grapes<-data.frame(original=NA, replacement=NA)
    unmatched.grapes=grape.range.df$mac
    persistent.grapes=grape.range.df$mac[which(grape.range.df$min==total.range$min & grape.range.df$max==total.range$max)]
    unmatched.grapes<-unmatched.grapes[-which(unmatched.grapes %in% persistent.grapes)]

    original.grapes=grape.range.df$mac[which(grape.range.df$min==total.range$min)]

    start.grapes = unmatched.grapes[which(unmatched.grapes %in% grape.range.df$mac[which(grape.range.df$min==total.range$min & !grape.range.df$max==total.range$max)])]

    i<-1
    while(length(unmatched.grapes)!=0){
        if(while.loop.df[unmatched.grapes[i]])
            temp.grape = while.loop.df[which(while.loop.df$mac== unmatched.grapes[i]),]

        possible.matches= grape.range.df[which(grape.range.df$min > temp.grape$max),]

        as.Date()

        if(length(possible.matches$mac)==0){
            paired.row = data.frame(original=temp.grape$mac, replacement=NA)
            paired.grapes = rbind(paired.grapes, paired.row)
        }else{
            paired.row = data.frame(original=temp.grape$mac, replacement= possible.matches$mac[1])
            paired.grapes = rbind(paired.grapes, paired.row)
        }
        unmatched.grapes= unmatched.grapes[-i]
        i = i+1
    }

    for(i in 1:length(unmatched.grapes)){
        print(i)
        temp.grape = grape.range.df[which(grape.range.df$mac== unmatched.grapes[i]),]
        possible.matches= grape.range.df[which(grape.range.df$min > temp.grape$max),]

        if(length(possible.matches$mac)==0){
            paired.row = data.frame(original=temp.grape$mac, replacement=NA)
            paired.grapes = rbind(paired.grapes, paired.row)
        }else{
            paired.row = data.frame(original=temp.grape$mac, replacement= possible.matches$mac)
            paired.grapes = rbind(paired.grapes, paired.row)
        }
        unmatched.grapes= unmatched.grapes[-i]
    }
}


