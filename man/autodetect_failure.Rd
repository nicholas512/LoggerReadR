% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HOBO.R
\name{autodetect_failure}
\alias{autodetect_failure}
\title{Notify autodetection failure}
\usage{
autodetect_failure(configuration)
}
\arguments{
\item{configuration}{A named list of file configuration properties according to \code{\link{detect_hoboware_configuration}}.
If missing, LoggerReadR will attempt to auto-detect configuration properties.}
}
\description{
Print best-guess configuration and notify that it couldn't find everything.
}
