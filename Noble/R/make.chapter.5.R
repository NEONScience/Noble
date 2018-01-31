############################################################################################
#' @title  Make Chapter 5 Summary RMD

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description For a data frame of unique site results ('parsed results'), RMD code will be generated
#' for the CTR.
#'
#' @param \code{parsed.results} The parsed version of the 'results.csv' in the SCA, generated with
#' \code{Noble::parse.results}.
#'
#' @return RMD code for Chapter 5 of TIS CTRs.

#' @keywords process quality, data quality, gaps, commissioning

#' @examples
#' None

#'
#' @seealso Currently none

# changelog and author contributions / copyrights
#   Robert Lee (2018-01-15)
#     original creation
#
##############################################################################################


make.chapter.5=function(parsed.results){
    resultLeng <<- length(parsed.results$site)
    i=0

    # Loop to write Section 5 with the right data. Requires the child .rmd file "chapt5results.rmd" be in the same directory
    # as this .Rmd file.

    for (i in 1:resultLeng) {
        out = NULL #reset the output of the loop to null, so duplicates aren't printed

        #---------- Site and variable values -----------#
        currSite <<- as.character(parsed.results$site[i]) # set the current site being examined (in order in results file)
        currDP <<- as.character(parsed.results$data_product[i]) # Current data product
        currDatVar <<- parsed.results$variable_tested[i] # Data variable tested on
        currDomn <<- Noble::tis_site_config$Domain[ Noble::tis_site_config$SiteID==currSite] # Find the corresponding domain number

        #------------- Time-related values -------------#
        currBgnDate <<- zoo::as.yearmon(parsed.results$begin_month[i]) # set the current commissioning start date (in order in results file)
        currEndDate <<- zoo::as.yearmon(parsed.results$end_month[i]) # set the current commissioning end date (in order in results file)
        userSpan <<- parsed.results$days_tested[i]
        currNumbDay <<- userSpan

        #----------- Testing-related values ------------#
        currPcntPass <<- as.numeric(parsed.results$data_validity[i])
        currPcntQuant <<-as.numeric(parsed.results$data_quantity[i])
        currTestDate <<- (parsed.results$time_performed[i])

        #------------- File-related values -------------#
        dataDir <<- paste(dirCommBase, testSubDir,"/", currDomn, "-", currSite, "/", sep="") # set file path to look for test file
        fileList <<- unlist(list.files(dataDir)) # get list of files to find from
        dataFiles<<-fileList[grep(pattern=currDP, fileList)]
        if(length(dataFiles)>0){
            fileDeets <<- file.info(paste(dataDir, dataFiles, sep="/"))
            fileDeets <<- data.frame(cbind(dataFiles, fileDeets))
            fileDeets <<- fileDeets[with(fileDeets, order(atime)), ]
            fileUsed <<- fileDeets$dataFiles[which.max(fileDeets$atime)]}else{fileUsed="NA"}
        #dataFile = fileList[grep(dataFile1, pattern=currBgnDate)]
        #dataFile = dataFile[-1]
        fullLoc <<- paste0(currDomn, "-", parsed.results$site[i]) # Make full site code
       # excluder(exFile=paste(dataDir, "exclude.csv", sep=""), dpID = currDPID)

        #------------- Set Test thresholds -------------#
        availTH <<- parsed.results$quant_threshold[i]#, round((parsed.results$quant_threshold[i] - (maintenance/(currNumbDay)*100)), digits = 2)
        validTH <<- parsed.results$valid_threshold[i]# round((parsed.results$valid_threshold[i] - (maintenance/(currNumbDay)*100)-(adverse/currNumbDay*100)), digits = 2)

        #------------- Output the results --------------#
        out = c(out, knitr::knit_expand('chap5result.Rmd')) ## Output chapter 5 RMD code with data to master RM
        return(out)
    }
}
