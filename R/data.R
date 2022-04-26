#' Kodebok for utledete variabler
#'
#' Datasett som inneholder forklaringer til alle utledete variabler.
#'
#' \itemize{
#' \item \code{rscript} Hvilket R-script funksjonen befinner seg i
#' \item \code{rfunksjon} Hvilken R-funksjon som lager den utledete variabelen
#' \item \code{skjemanavn} Alltid "utledet_variabel"
#' \item \code{fysisk_feltnavn} Navn på variabelen i tabellen/utforsker
#' \item \code{ledetekst} Forklaring til variabelen
#' \item \code{listeverdier} Mulige verdier for kategoriske variabler
#' \item \code{listetekst} Forklaring til hver verdi av de kategoriske
#' variablene
#' \item \code{siste oppdatering} Når ble variabelen sist oppdatert i kodeboken
#' }
#'
#' @format data.frame med 9 kolonner og en rad per utledet variabel.

"def_utledete_var"