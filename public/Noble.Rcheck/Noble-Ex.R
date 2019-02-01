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
##D tempDir= tempdir()
##D pull.data(site = "CPER",
##D dp.id = "DP1.00002.001",
##D bgn.month = "2017-04",
##D end.month = "2017-05",
##D time.agr = 30,
##D package="basic",
##D save.dir= tempDir)
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
##D data=Noble::pull.data(site = "CPER", dp.id = "DP1.00001.001", bgn.month = "2017-08",
##D end.month = "2017-08", time.agr = 30, save.dir = tempdir())
##D ## Extract data from Aug 8th, UTC.
##D aug8=date.extract(data=data, bgn.date = "2017-08-08", end.date = "2017-08-09")
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
nameEx("dp.survey.plot")
### * dp.survey.plot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dp.survey.plot
### Title: Save Summary Graphs of Data Product Health by Month
### Aliases: dp.survey.plot
### Keywords: commissioning, data gaps, health process product, quality,

### ** Examples

## Not run: 
##D # For 2d Wind, save all plots to the current working directory:
##D dp.survey(dp.id = "DP1.00001.001", save.dir = getwd())
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dp.survey.plot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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

## Not run: 
##D # For 2d Wind, save files to the current working directory:
##D gap.report(site="CPER", dp.id = "DP1.00001.001", bgn.month="2017-07", end.month="2017-07", save.dir = getwd())
## End(Not run)



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
##D gap.vis(site="CPER", dp.id = "DP1.00001.001", bgn.month="2017-07", end.month="2017-07", save.dir = getwd())
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
CPER_wind=dp.survey(dp.id = "DP1.00001.001", site="CPER")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("health.data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("neon.avail")
### * neon.avail

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: neon.avail
### Title: Returns a data frame of data product availability by month
### Aliases: neon.avail
### Keywords: commissioning data gaps, process quality,

### ** Examples

## Not run: 
##D wind<-neon.avail(dp.id = "DP1.00001.001")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("neon.avail", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
nameEx("pull.data")
### * pull.data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pull.data
### Title: Downloads data for a specified data product or products, and
###   saves the data to a specified directory
### Aliases: pull.data
### Keywords: commissioning data gaps, process quality,

### ** Examples

## Not run: 
##D #Make a temporary directory for the example:
##D tempDir<- tempdir()
##D pull.data(site = "CPER", dp.id = "DP1.00002.001", bgn.month = "2017-04", end.month = "2017-05",
##D time.agr = 30, package="basic", save.dir= tempDir)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pull.data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
##D pull.date(site = "CPER", dp.id = "DP1.00002.001", bgn.month = "2017-03-15 00:00:00",
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

## Not run: 
##D 2d_wind_locs=Noble::pull.dp.locs(site="CPER", dp.id="DP1.00001.001")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pull.dp.locs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pull.eddy.data")
### * pull.eddy.data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pull.eddy.data
### Title: Download NEON Eddy Covaraince Data
### Aliases: pull.eddy.data
### Keywords: commissioning covariance, data eddy gaps, hdf5, process
###   quality,

### ** Examples

## Not run: 
##D site="CPER"
##D bgn.month="2017-04"
##D end.month="2017-11"
##D package="basic"
##D save.dir=tempdir()
##D pull.eddy.data(site, bgn.month, end.month, package, save.dir)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pull.eddy.data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pull.n.plot")
### * pull.n.plot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pull.n.plot
### Title: Returns PDF(s) of data for the specified site and data product
### Aliases: pull.n.plot
### Keywords: commissioning data gaps, process quality,

### ** Examples

## Not run: 
##D # for a variable, "test.dir", holding a valid file path:
##D pull.n.plot(bgn.month = "2017-04",
##D end.month = "2017-05",
##D dp.id = "DP1.00001.001",
##D sites.req = "BLAN",
##D save.dir = getwd(),
##D  data.field = "windDirMean")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pull.n.plot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
### Title: Perform A Commissioning PQ Test on 3D Sonic Anemometer Data
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
##D wind=test.sites(dpId="DP1.00001.001")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("test.sites", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("wind.dir.hist")
### * wind.dir.hist

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: wind.dir.hist
### Title: Plot 2D Wind Direction Histograms
### Aliases: wind.dir.hist
### Keywords: air commissioning, consistency, data process quality,
###   temperature

### ** Examples

## Not run: 
##D plot.wind.dir.hist(site="CPER", bgn.month="2017-05", end.month="2017-05", save.dir=getwd())
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("wind.dir.hist", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("wind.rose.plot")
### * wind.rose.plot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: wind.rose.plot
### Title: Create wind roses for NEON instrumented sites
### Aliases: wind.rose.plot
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
base::cat("wind.rose.plot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
