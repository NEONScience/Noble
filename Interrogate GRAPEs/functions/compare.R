require(ggplot2)

files=locate(site="heal", bgn = "2017-10-8", end="2017-12-01")

out=lapply(files, function(f) marry(log.file=f[1], map.file = f[2]))

#so that out is permanent-ish
temp=out

gr=lapply(temp, "[[", "grape")

names(gr)=lapply(temp, "[[", "date")

items.connected=data.frame(do.call(cbind, lapply(gr, function(d) lapply(d, function(g) length(g$NAME)))))

col.names=gsub(pattern = "X", replacement = "", x = colnames(items.connected))

items.connected=data.frame(MAC=rownames(items.connected), lapply(seq(length(items.connected)), function(i) as.numeric(items.connected[,i])))

colnames(items.connected)=c("MAC", col.names)
m.items=reshape2::melt(items.connected, id.vars="MAC")
colnames(m.items)=c("MAC", "Date", "Connections")
m.items$Date=as.Date(m.items$Date, format="%Y.%m.%d")
m.items$Connections=as.integer(m.items$Connections)


plotly::ggplotly(ggplot(data=m.items, aes(x=Date, y = Connections, color=factor(MAC)))+geom_path())

class(items.connected$X2017.07.01)



sapply(items.connected[,2:length(items.connected)], class(x), "numeric")
