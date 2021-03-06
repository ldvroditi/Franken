% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Franken.R
\name{AddFlowFramefrkn}
\alias{AddFlowFramefrkn}
\title{Add a flowFrame to the data variable of the flowSOM object}
\usage{
AddFlowFramefrkn(fsom, flowFrame)
}
\arguments{
\item{fsom}{flowSOM object, as constructed by the ReadInputfrkn function}

\item{flowFrame}{flowFrame to add to the flowSOM object}
}
\value{
flowSOM object with data added
}
\description{
Modified version of AddFlowFrame from flowSOM to accept data.frame or data.table.
Beware that feeding data.frame or data.table to Franken means you might be missing some meta-data usually found in the originall .fcs files.
}
