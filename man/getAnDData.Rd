% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getAnDData.R
\name{getAnDData}
\alias{getAnDData}
\title{getAnDData provides local reg data from AnnenDiagnostikkVar}
\usage{
getAnDData(registryName, singleRow = FALSE, ...)
}
\arguments{
\item{registryName}{String providing the registry name}

\item{singleRow}{Logical defining if only one row is to be returned. A
relevant usecase will be when only description is needed. By default set to
FALSE}

\item{...}{Optional arguments to be passed to the function}
}
\value{
Data frame representing the table AnnenDiagnostikkVar
}
\description{
getAnDData provides local reg data from AnnenDiagnostikkVar
}
