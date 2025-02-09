pack.files=list.files("./R/")


## fix the new data.pull name
for(f in 1:length(pack.files)){
    file=paste0("./R/", pack.files[f])
    contents=readLines(file)
    new.contents=gsub(pattern = "data.pull", replacement = "pull.data", x = contents)
    write(x=new.contents, file=file)
 }

## change to dp.id from dpID
for(f in 1:length(pack.files)){
    file=paste0("./R/", pack.files[f])
    contents=readLines(file)
    new.contents=gsub(pattern = "dpID", replacement = "dp.id", x = contents, ignore.case = F)
    write(x=new.contents, file=file)
}

## change to end.day.time to prevent S3 issues
# for(f in 1:length(pack.files)){
#     file=paste0("./R/", pack.files[f])
#     contents=readLines(file)
#     new.contents=gsub(pattern = "end.day.time", replacement = "last.day.time", x = contents, ignore.case = F)
#     write(x=new.contents, file=file)
# }

## NEON to neon in function name
# for(f in 1:length(pack.files)){
#     file=paste0("./R/", pack.files[f])
#     contents=readLines(file)
#     new.contents=gsub(pattern = "NEON.avail", replacement = "neon.avail", x = contents, ignore.case = F)
#     write(x=new.contents, file=file)
# }

## NEON to neon in function name
# for(f in 1:length(pack.files)){
#     file=paste0("./R/", pack.files[f])
#     contents=readLines(file)
#     new.contents=gsub(pattern = "find.gap", replacement = "gap.find", x = contents, ignore.case = F)
#     write(x=new.contents, file=file)
# }




 ## NEON to neon in function name
for(f in 1:length(pack.files)){
    file=paste0("./R/", pack.files[f])
    contents=readLines(file)
    new.contents=gsub(pattern = "#' @seealso Currently none", replacement = "", x = contents, ignore.case = F)
    write(x=new.contents, file=file)
}

 ## NEON to neon in function name
for(f in 1:length(pack.files)){
    file=paste0("./R/", pack.files[f])
    contents=readLines(file)
    new.contents=gsub(pattern = "base::write.csv", replacement = "utils::write.csv", x = contents, ignore.case = F)
    write(x=new.contents, file=file)
}

## NEON to neon in function name
for(f in 1:length(pack.files)){
    file=paste0("./R/", pack.files[f])
    contents=readLines(file)
    new.contents=gsub(pattern = "base::read.csv", replacement = "utils::read.csv", x = contents, ignore.case = F)
    write(x=new.contents, file=file)
}

## NEON to neon in function name
for(f in 1:length(pack.files)){
    file=paste0("./R/", pack.files[f])
    contents=readLines(file)
    new.contents=gsub(pattern = "GHCNDstations", replacement = "USCRN_sites", x = contents, ignore.case = F)
    write(x=new.contents, file=file)
}


