#' Hent mergede og behandlede datasett
#'
#' @param registryName String providing the registry name
#' @param fromDate A Date or character class object  of format "YYYY-MM-DD"
#' defining start of the period from which data will be collected. Default is
#' \code{NULL} in which case til will be set to the assumed pre-historic date of
#' "1900-01-01".
#' @param toDate A Date or character class object  of format "YYYY-MM-DD"
#' defining end of the period from which data will be collected. Default is
#' \code{NULL} in which case til will be set to the last known registration date
#' for NORIC.
#' @param singleRow Logical defining if only one row is to be returned. A
#' relevant usecase will be when only description is needed. By default set to
#' FALSE
#' @param ... Optional arguments to be passed to the function
#' @importFrom dplyr filter mutate mutate_all select left_join
#' @importFrom lubridate ymd year month quarter isoweek
#'n
#' @return Data frame representing the chosen table. Basic data managment is
#' done (e.g. added variables from FO, added time-variables, rename hospitals)
#' @export
#' @aliases getPrepApData getPrepSOData
#' NULL
#'
getPrepApData <- function(registryName, fromDate, toDate, singleRow,...){


  . <- ""

  # Hente AP-tabell med utvalgte variabler fra FO
  dataListe <- noric::getAp(registryName = registryName,
                            fromDate = fromDate,
                            toDate = toDate,
                            singleRow = singleRow)
  aP <- dataListe$aP


  # Gjor datoer om til dato-objekt:
  aP %<>%
    dplyr::mutate_at(vars(ends_with("dato", ignore.case = TRUE)),
                     list(ymd))


  # Endre Sykehusnavn til kortere versjoner:
  aP %<>% noric::fikse_sykehusnavn(df = .)


  # Tar bort forløp fra før sykehusene ble offisielt med i NORIC
  # (potensielle "tøyseregistreringer")
  aP %<>% noric::fjerne_tulleregistreringer(df = ., var = ProsedyreDato)


  # Legg til aar, maaned, uke, etc.
  aP %<>% noric::legg_til_tidsvariabler(df = ., var = ProsedyreDato)

  aP
}


getPrepSoData <- function(registryName, fromDate, toDate, singleRow,...){


  . <- ""

  # Hente So-tabell med utvalgte variabler fra FO
  dataListe <- noric::getSo(registryName = registryName,
                            fromDate = fromDate,
                            toDate = toDate,
                            singleRow = singleRow)
  sO <- dataListe$sO


  # Gjor datoer om til dato-objekt:
  sO %<>% dplyr::mutate(
    OpprettetDato = lubridate::ymd_hms(.data$OpprettetDato),
    SistLagretDato = lubridate::ymd_hms(.data$SistLagretDato),
    HovedDato = lubridate::ymd(.data$HovedDato))


  # Endre Sykehusnavn til kortere versjoner:
  sO %<>% noric::fikse_sykehusnavn(df = .)


  # Tar bort forløp fra før sykehusene ble offisielt med i NORIC (potensielle
  # "tøyseregistreringer")
  sO %<>% noric::fjerne_tulleregistreringer(df = ., var = ProsedyreDato)


  # Utledete variabler:
  sO %<>% dplyr::mutate(
    # Ferdigstilt, 1= ja, -1 & 0 = nei
    ferdigstilt = as.ordered(ifelse(.data$SkjemaStatus == 1,
                                    yes = "Ferdigstilt",
                                    no = "Ikke ferdigstilt")))



  # Legg til aar, maaned, uke, etc.
  sO %<>% noric::legg_til_tidsvariabler(df = ., var = ProsedyreDato)

  sO
}

