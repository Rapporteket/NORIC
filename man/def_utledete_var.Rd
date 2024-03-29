% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{def_utledete_var}
\alias{def_utledete_var}
\title{Kodebok for utledete variabler}
\format{
data.frame med 9 kolonner og en rad per utledet variabel.
}
\usage{
def_utledete_var
}
\description{
Datasett som inneholder forklaringer til alle utledete variabler.
}
\details{
\itemize{
\item \code{rscript} Hvilket R-script funksjonen befinner seg i
\item \code{rfunksjon} Hvilken R-funksjon som lager den utledete variabelen
\item \code{skjemanavn} Alltid "utledet_variabel"
\item \code{fysisk_feltnavn} Navn på variabelen i tabellen/utforsker
\item \code{ledetekst} Forklaring til variabelen
\item \code{listeverdier} Mulige verdier for kategoriske variabler
\item \code{listetekst} Forklaring til hver verdi av de kategoriske
variablene
\item \code{siste oppdatering} Når ble variabelen sist oppdatert i kodeboken
}
}
\keyword{datasets}
