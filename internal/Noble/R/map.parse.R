map.dir="~/Documents/maps/CPER/"



grape.fail=function(map.dir){
    map.parse=function(map.dir){
        map=read.csv(map.dir)
        parsed.map=unique(data.frame(map$PART., map$MAC, map$DESC))
        return(parsed.map)
    }

    lost.assemblies=data.frame(Assembly=c(), GRAPE=c(), Location=c(), Date=c())

    dates=stringr::str_extract(string = list.files(path = map.dir), pattern = "\\d{8}")
    site.sums=lapply(list.files(path = map.dir, full.names = T), function(x) data.frame(map.parse(x)))

    fails=lapply(site.sums, function(x) length(grep(pattern = "FF:FF", x = x)))

    #names(fails)=dates

    fail.df=data.frame(Date=as.POSIXct(dates, format="%Y%m%d")-lubridate::days(1) , "Failed.GRAPEs"=do.call(rbind, fails))

    fail.indx=which(fail.df$Failed.GRAPEs>0)
    pass.indx=which(!(seq(length(fail.df$Date)) %in% fail.indx))


    # Should look at pass.indx for breaks in instrument availability!!!! THEN DECIDE whether multiple good maps are needed!
    if(length(fail.indx)>0){

        loc.find=function(x){
            map=read.csv(x)
            loc=map$LOC[grepl(pattern = "*:FF:FF:FF",x= map$MAC)]
        }

        fail.files=list.files(path = map.dir, full.names = T)[fail.indx]
        fail.loc=lapply(fail.files, function(f) loc.find(f))

        fail.df$Location=rep(NA, times=length(fail.df$Date))

        fail.df$Location[fail.df$Failed.GRAPEs>0]=unlist(fail.loc)

        ggplot2::ggplot(data=fail.df, ggplot2::aes(x=Date, y=Failed.GRAPEs, color=factor(Location)))+
            ggplot2::geom_line()+
            ggplot2::theme_light()

        good.map=read.csv(list.files(path = map.dir, full.names = T)[pass.indx[length(pass.indx)]])



        for( i in 1:length(fail.indx)){
            bad.map= read.csv(list.files(path = map.dir, full.names = T)[fail.indx[i]])
            missing.desc=good.map$DESC[!(good.map$IP %in% bad.map$IP)]
            temp=data.frame(Assembly=missing.desc, Location=good.map$LOC[!(good.map$IP %in% bad.map$IP)], GRAPE=good.map$MAC[!(good.map$IP %in% bad.map$IP)], Date=fail.df$Date[fail.indx[i]])
            lost.assemblies=rbind(lost.assemblies, temp)
        }
    }
    return(lost.assemblies)
}
