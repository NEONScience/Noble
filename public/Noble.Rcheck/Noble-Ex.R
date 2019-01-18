pkgname <- "Noble"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "Noble-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('Noble')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("NEON.avail")
### * NEON.avail

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: NEON.avail
### Title: Returns a data frame of data product availability by month
### Aliases: NEON.avail
### Keywords: commissioning data gaps, process quality,

### ** Examples

## Not run: 
##D wind<-NEON.avail(dpID = "DP1.00001.001")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("NEON.avail", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("air.temp.cnst.plot")
### * air.temp.cnst.plot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: air.temp.cnst.plot
### Title: Generate Pairwise Comparison Plots for SAAT Measurements by ML
### Aliases: air.temp.cnst.plot
### Keywords: air commissioning, consistency, data process quality,
###   temperature

### ** Examples

air.temp.cnst.plot(site="CPER", bgn.month="2017-05", end.month="2017-05", save.dir=getwd())




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("air.temp.cnst.plot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("air.temp.dq.test")
### * air.temp.dq.test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: air.temp.dq.test
### Title: Test NEON Air Temperature data for Stability of Variance,
###   Internal Consistency, and External Consistency.
### Aliases: air.temp.dq.test
### Keywords: commissioning data gaps, process quality,

### ** Examples

## Not run: 
##D #Make a temporary direcotry for the example:
##D tempDir<- tempdir()
##D data.pull(site = "CPER", dpID = "DP1.00002.001", bgn.month = "2017-04", end.month = "2017-05", time.agr = 30, package="basic", save.dir= tempDir)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("air.temp.dq.test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("air.temp.plot")
### * air.temp.plot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: air.temp.plot
### Title: Air Temperature Plot
### Aliases: air.temp.plot
### Keywords: data, date, subset,

### ** Examples

## Not run: 
##D save.dir=getwd()
##D bgn.month<-"2016-01"
##D end.month<-"2016-04"
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("air.temp.plot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("co2.pq.test")
### * co2.pq.test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: co2.pq.test
### Title: Perform A Commissioning PQ Test on IRGA CO2 Concentration Data
###   #' @author Robert Lee <email: rlee@battelleecology.org>
### Aliases: co2.pq.test
### Keywords: commissioning data gaps, process quality,

### ** Examples

site = "CPER"
bgn.month = "2017-05"
end.month = "2017-06"
time.agr = 30
package="basic"
save.dir<-tempdir()
Noble::tis.pq.test(site = site, bgn.month = bgn.month, end.month = end.month, save.dir=save.dir)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("co2.pq.test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("date.extract")
### * date.extract

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: date.extract
### Title: Date Extract
### Aliases: date.extract
### Keywords: data, date, subset,

### ** Examples

## Not run: 
##D data=Noble::data.pull(site = "CPER", dpID = "DP1.00001.001", bgn.month = "2017-08",
##D end.month = "2017-08", time.agr = 30, save.dir = tempdir())
##D ## Extract data from Aug 8th, UTC.
##D aug8=ml.extract(data=data, bgn.date = "2017-08-08", end.date = "2017-08-09")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("date.extract", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("distorted.field")
### * distorted.field

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: distorted.field
### Title: Distorted Field Calculations
### Aliases: distorted.field
### Keywords: 2D data distorted flow, quality wind,

### ** Examples

## Not run: 
##D out=distorted.field("CPER")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("distorted.field", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dp.survey")
### * dp.survey

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dp.survey
### Title: Save Summary Graphs of Data Product Health by Month
### Aliases: dp.survey
### Keywords: commissioning, data gaps, health process product, quality,

### ** Examples

# For 2d Wind, save all plots to the current working directory:
dp.survey(dpID = "DP1.00001.001", save.dir = getwd())



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dp.survey", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("fan.test")
### * fan.test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fan.test
### Title: Tests fan aspiration system performance
### Aliases: fan.test
### Keywords: commissioning data gaps, process quality,

### ** Examples

Currently none



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fan.test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("find.gap")
### * find.gap

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: find.gap
### Title: Returns indicies for missing NEON data
### Aliases: find.gap
### Keywords: commissioning data gaps, process quality,

### ** Examples

Currently none



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("find.gap", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gap.report")
### * gap.report

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gap.report
### Title: Generate and Write Data Product Gap Summaries
### Aliases: gap.report
### Keywords: commissioning, data gaps, health process product, quality,

### ** Examples

# For 2d Wind, save files to the current working directory:
gap.report(site="CPER", dpID = "DP1.00001.001", bgn.month="2017-07", end.month="2017-07", save.dir = getwd())



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gap.report", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gap.vis")
### * gap.vis

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gap.vis
### Title: Generate Data Product Gap Visualization
### Aliases: gap.vis
### Keywords: commissioning, data gaps, health process product, quality,

### ** Examples

## Not run: 
##D # For 2d Wind, save files to the current working directory:
##D gap.vis(site="CPER", dpID = "DP1.00001.001", bgn.month="2017-07", end.month="2017-07", save.dir = getwd())
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gap.vis", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("health.data")
### * health.data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: health.data
### Title: Produce a Summary Table of Data Product Health by Month
### Aliases: health.data
### Keywords: commissioning, data gaps, health process product, quality,

### ** Examples

# Summarize 2D wind perfomance at CPER:
CPER_wind=dp.survey(dpID = "DP1.00001.001", site="CPER")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("health.data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("par.cnst.plot")
### * par.cnst.plot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: par.cnst.plot
### Title: Generate Pairwise Comparison Plots for PAR by ML
### Aliases: par.cnst.plot
### Keywords: air commissioning, consistency, data process quality,
###   temperature

### ** Examples

## Not run: 
##D par.cnst.plot(site="CPER", bgn.month="2017-05", end.month="2017-05", save.dir=getwd())
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("par.cnst.plot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("parse.results")
### * parse.results

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: parse.results
### Title: Parse a Raw Commissioning Results File for Most Recent Results
### Aliases: parse.results
### Keywords: commissioning data gaps, process quality,

### ** Examples

None




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("parse.results", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.dp.survey")
### * plot.dp.survey

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.dp.survey
### Title: Save Summary Graphs of Data Product Health by Month
### Aliases: plot.dp.survey
### Keywords: commissioning, data gaps, health process product, quality,

### ** Examples

# For 2d Wind, save all plots to the current working directory:
dp.survey(dpID = "DP1.00001.001", save.dir = getwd())



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.dp.survey", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.wind.dir.hist")
### * plot.wind.dir.hist

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.wind.dir.hist
### Title: Plot 2D Wind Direction Histograms
### Aliases: plot.wind.dir.hist
### Keywords: air commissioning, consistency, data process quality,
###   temperature

### ** Examples

## Not run: 
##D plot.wind.dir.hist(site="CPER", bgn.month="2017-05", end.month="2017-05", save.dir=getwd())
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.wind.dir.hist", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.wind.rose")
### * plot.wind.rose

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.wind.rose
### Title: Create wind roses for NEON instrumented sites
### Aliases: plot.wind.rose
### Keywords: commissioning data gaps, process quality,

### ** Examples

## Not run: 
##D CPER<-plot.wind.rose(site="CPER",
##D bgn.month="2017-01",
##D end.month="2017-02",
##D ml=2,
##D speed.bins=10,
##D dir.bins=36)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.wind.rose", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pull.USCRN.data")
### * pull.USCRN.data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pull.USCRN.data
### Title: Pull data from meterologic stations in the USCRN network
### Aliases: pull.USCRN.data
### Keywords: USCRN, commissioning data data, gaps, process quality,

### ** Examples

## Not run: 
##D timeScale <- "subhourly"
##D stationID <- "USW00003047"
##D TimeBgn <- as.POSIXct("2014-04-01 00:00:01", format="%Y-%m-%d %H:%M:%S", tz="UTC")
##D TimeEnd <- as.POSIXct("2015-02-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz="UCT")
##D 
##D grabUSCRN(timeScale = timeScale, TimeBgn = TimeBgn, TimeEnd = TimeEnd, stationID = stationID)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pull.USCRN.data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pull.date")
### * pull.date

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pull.date
### Title: Return TIS Data in a Specific Date-time Range
### Aliases: pull.date
### Keywords: commissioning data gaps, process quality,

### ** Examples

## Not run: 
##D #Make a temporary direcotry for the example:
##D tempDir<- tempdir()
##D pull.date(site = "CPER", dpID = "DP1.00002.001", bgn.month = "2017-03-15 00:00:00",
##D end.month = "2017-03-16 00:00:00", time.agr = 30, package="basic", save.dir= tempDir)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pull.date", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pull.dp.locs")
### * pull.dp.locs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pull.dp.locs
### Title: Return the Current Sensor Location Information for a Given Data
###   Product
### Aliases: pull.dp.locs
### Keywords: data, meta position sensor, spatial,

### ** Examples

2d_wind_locs=Noble::pull.dp.locs(site="CPER", dpID="DP1.00001.001")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pull.dp.locs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("soil.temp.plot")
### * soil.temp.plot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: soil.temp.plot
### Title: Plot Soil Temperature Profiles at a Given Time
### Aliases: soil.temp.plot
### Keywords: commissioning data gaps, process quality,

### ** Examples

## Not run: plot=Noble::soil.temp.plot(site="CPER", date="2018-02-28 12:30")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("soil.temp.plot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("soni.pq.test")
### * soni.pq.test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: soni.pq.test
### Title: Perform A Commissioning PQ Test on 3D Sonic Anemometer Data #'
###   @author Robert Lee <email: rlee@battelleecology.org>
### Aliases: soni.pq.test
### Keywords: commissioning data gaps, process quality,

### ** Examples

## Not run: 
##D site = "CPER"
##D bgn.month = "2017-05"
##D end.month = "2017-06"
##D time.agr = 30
##D package="basic"
##D save.dir<-tempdir()
##D Noble::tis.pq.test(site = site, bgn.month = bgn.month, end.month = end.month, save.dir=save.dir)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("soni.pq.test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("test.sites")
### * test.sites

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: test.sites
### Title: Returns a list of sites where a given data product exists in a
###   given time range
### Aliases: test.sites
### Keywords: commissioning data gaps, process quality,

### ** Examples

## Not run: 
##D 2d_wind=test.sites(dpId="DP1.00001.001")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("test.sites", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tis.pq.test")
### * tis.pq.test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tis.pq.test
### Title: Downloads and performs process quality checks on NEON data
### Aliases: tis.pq.test
### Keywords: commissioning data gaps, process quality,

### ** Examples

## Not run: 
##D site = "CPER"
##D dpID = "DP1.00001.001"
##D prin.vars<-c("windSpeed", "windDir")
##D bgn.month = "2017-05"
##D end.month = "2017-06"
##D time.agr = 30
##D package="basic"
##D save.dir<-"/Users/rlee/Dropbox/GitHub/Commissioning-TIS-rhlee12/Tis2DWindPQ_test"
##D Noble::tis.pq.test(site = site, dpID = dpID, bgn.month = bgn.month, end.month = end.month,
##D time.agr = time.agr, package=package, save.dir=save.dir, prin.vars=prin.vars)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tis.pq.test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
