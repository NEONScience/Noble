% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rad_dq_test.R
\name{rad.dq.test}
\alias{rad.dq.test}
\title{Execute Radiation Data Quality Testing}
\usage{
rad.dq.test(site, save.dir, bgn.month, end.month)
}
\arguments{
\item{\code{site}}{The TIS site of interest, as a 4-letter code.}

\item{\code{save.dir}}{The save directory for data, results, and other files.}

\item{\code{bgn.month}}{The first month of testing, as "YYYY-MM".}

\item{\code{end.month}}{The last month of testing, as "YYYY-MM".}
}
\value{
Site results in 'results.csv', and raw
}
\description{
Run 4 checks on radiation data quality for TIS sites: (1) Internal Correlation,
(2) Tower-Top Sensor Agreement, (3) Variance Stability, and
(4) External Correlation with a Non-NEON Site. Test results are written as simple pass/fail entries
in the results.csv files, however expanded test resullts are written in a site-specific folder in
the save directory.
}
\examples{
\dontrun{
site="BART"
bgn.month="2017-06"
end.month="2017-07"

testSubDir = ""
if(grepl("darwin", version$os))
{
    mountPoint<-"/Volumes/neon/" #Mac
}else{
    mountPoint<-"N:/" #Windows
}
testFullDir =  paste0(mountPoint, "Science/Science Commissioning Archive/SiteAndPayload/TisRadiationDataQuality/")
rad.dq.test(site=site, save.dir=testFullDir, bgn.month=bgn.month, end.month=end.month)
}

}
\seealso{
Currently none
}
\author{
Robert Lee \email{rlee@battelleecology.org}\cr
}
\keyword{commissioning}
\keyword{data}
\keyword{gaps,}
\keyword{process}
\keyword{quality,}
