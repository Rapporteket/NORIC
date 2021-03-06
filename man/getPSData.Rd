% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getPSData.R
\name{getPSData}
\alias{getPSData}
\title{getPSData provides local or national reg data from PasienterStudier}
\usage{
getPSData(registryName, singleRow = FALSE, ...)
}
\arguments{
\item{registryName}{String providing the registry name}

\item{singleRow}{Logical defining if only one row is to be returned. A
relevant usecase will be when only description is needed. By default set to
FALSE}

\item{...}{Optional arguments to be passed to the function}
}
\value{
Data frame representing the table PasienterStudier
}
\description{
getPSData provides local or national reg data from PasienterStudier
}
