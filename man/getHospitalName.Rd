% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getHospitalName.R
\name{getHospitalName}
\alias{getHospitalName}
\title{Get hospital name from registry data}
\usage{
getHospitalName(regName, national = FALSE)
}
\arguments{
\item{regName}{string defining the db}

\item{national}{true or false}
}
\value{
string of hospital name
}
\description{
Based on the hospital id (reshID) this function will return the name of
the corresponding hospital as provided in the registry data
}
\examples{
\dontrun{
getHospitalName("my_db")
}
}
