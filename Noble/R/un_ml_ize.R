#data=example.data

un.ml.ize=function(data, keep.na){
    if(missing(keep.na)){keep.na=T}
    if(!is.logical(keep.na)){message("keep.n is not logical, defaulting to TRUE.")
        keep.na=T}
    data=data[,-2]
    data.melt=reshape2::melt(data, id.vars="startDateTime")
    data.melt$variable=stringr::str_replace(string = data.melt$variable, pattern = "\\.\\d\\d\\d\\.\\d\\d\\d", replacement = "")
    if(keep.na==F){data.melt=data.melt[-is.na(data.melt$value),]}

    data.out=reshape2::dcast(data = data.melt, formula = startDateTime~variable, fun.aggregate = mean)
    return(data.out)
}
