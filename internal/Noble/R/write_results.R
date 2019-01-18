.write.results=function(result, save.dir){
    if(file.exists(Noble:::.result.route(save.dir))){
        temp.result = data.frame(read.csv(file = Noble:::.result.route(save.dir), header = T, stringsAsFactors = F))
        dq.rpt = rbind(temp.result, result)
        write.csv(x = dq.rpt, file = Noble:::.result.route(save.dir), row.names = F)
    }
    else{
        write.csv(x = result, file = Noble:::.result.route(save.dir), col.names = T, row.names = F)
    }
}
