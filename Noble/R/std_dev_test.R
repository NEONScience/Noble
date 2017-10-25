std.dev.test = function(data, data.field){
    data.index = grep(data.field, colnames(data), ignore.case = T)
    vector.data = unlist(as.list(data[,data.index]))
    std.dev = round(stats::sd(x = vector.data, na.rm = T), digits = 2)
    return(std.dev)
}

variance.test = function(pop.1, pop.2, data.field){
    # First population
    pop.1.index = grep(data.field, colnames(pop.1), ignore.case = T)
    vector.1 = unlist(as.list(data[,pop.1.index]))

    #Second population
    pop.2.index = grep(data.field, colnames(pop.2), ignore.case = T)
    vector.2 = unlist(as.list(data[,pop.2.index]))

    variance<-var.test(x=vector.1, y=vector.2, conf.level = .99)
    return(variance)
}

