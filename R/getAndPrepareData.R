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
#'
#' @return Data frame representing the chosen table. Basic data managment is
#' done (e.g. added variables from FO, added time-variables, rename hospitals)
#' @export
#' @aliases getPrepApData getPrepSOData getPrepAkData getPrepFoData getPrepAnPData
#' NULL
#'
getPrepApData <- function(registryName, fromDate, toDate, singleRow,...){


  . <- ""

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
    ferdigstilt = as.ordered(ifelse(.data$SkjemaStatus == 1,
                                    yes = "Ferdigstilt",
                                    no = "Ikke ferdigstilt")))



  # Legg til aar, maaned, uke, etc.
  sO %<>% noric::legg_til_tidsvariabler(df = ., var = ProsedyreDato)

  sO
}


getPrepAkData <- function(registryName, fromDate, toDate, singleRow,...){


  . <- ""

  dataListe <- noric::getAk(registryName = registryName,
                            fromDate = fromDate,
                            toDate = toDate,
                            singleRow = singleRow)
  aK <- dataListe$aK


  # Gjor datoer om til dato-objekt:
  aK %<>%
    dplyr::mutate_at(vars(ends_with("dato", ignore.case = TRUE)),
                     list(ymd))


  # Endre Sykehusnavn til kortere versjoner:
  aK %<>% noric::fikse_sykehusnavn(df = .)


  # Tar bort forløp fra før sykehusene ble offisielt med i NORIC
  # (potensielle "tøyseregistreringer")
  aK %<>% noric::fjerne_tulleregistreringer(df = ., var = ProsedyreDato)


  # Legg til aar, maaned, uke, etc.
  aK %<>% noric::legg_til_tidsvariabler(df = ., var = ProsedyreDato)


  # utlede variabler
  aK %<>% dplyr::mutate(
    dager_mellom_prosedyre_og_utskr = as.numeric(difftime(.data$UtskrDato,
                                                          .data$ProsedyreDato,
                                                          units = "days")))


  aK
}

getPrepFoData <- function(registryName, fromDate, toDate, singleRow,...){


  . <- ""

  dataListe <- noric::getFo(registryName = registryName,
                            fromDate = fromDate,
                            toDate = toDate,
                            singleRow = singleRow)
  fO <- dataListe$fO


  # Gjor datoer om til dato-objekt:
  fO %<>%
    dplyr::mutate_at(vars(ends_with("dato", ignore.case = TRUE)),
                     list(ymd))


  # Endre Sykehusnavn til kortere versjoner:
  fO %<>% noric::fikse_sykehusnavn(df = .)


  # Tar bort forløp fra før sykehusene ble offisielt med i NORIC (potensielle
  # "tøyseregistreringer")
  fO %<>% noric::fjerne_tulleregistreringer(df = ., var = HovedDato)



  # Legg til aar, maaned, uke, etc.
  fO %<>% noric::legg_til_tidsvariabler(df = ., var = HovedDato)

  fO
}

getPrepAnPData <- function(registryName, fromDate, toDate, singleRow,...){


  . <- ""

  dataListe <- noric::getAnP(registryName = registryName,
                             fromDate = fromDate,
                             toDate = toDate,
                             singleRow = singleRow)
  anP <- dataListe$anP


  # Gjor datoer om til dato-objekt:
  anP %<>%
    dplyr::mutate_at(vars(ends_with("dato", ignore.case = TRUE)),
                     list(ymd))


  # Endre Sykehusnavn til kortere versjoner:
  anP %<>% noric::fikse_sykehusnavn(df = .)


  # Tar bort forløp fra før sykehusene ble offisielt med i NORIC
  # (potensielle "tøyseregistreringer")
  anP %<>% noric::fjerne_tulleregistreringer(df = ., var = ProsedyreDato)


  # Legg til aar, maaned, uke, etc.
  anP %<>% noric::legg_til_tidsvariabler(df = ., var = ProsedyreDato)

  # Gjøre kategoriske variabler om til factor:
  anP %<>%
    dplyr::mutate(
      ForlopsType2 = factor(.data$ForlopsType2,
                            levels = c("Akutt",
                                       "Subakutt",
                                       "Planlagt"),
                            ordered = TRUE))

  anP
}


getPrepCtData <- function(registryName, fromDate, toDate, singleRow,...){


  . <- ""

  dataListe <- noric::getCt(registryName = registryName,
                             fromDate = fromDate,
                             toDate = toDate,
                             singleRow = singleRow)
  cT <- dataListe$cT


  # Gjor datoer om til dato-objekt:
  cT %<>%
    dplyr::mutate_at(vars(ends_with("dato", ignore.case = TRUE)),
                     list(ymd))


  # Endre Sykehusnavn til kortere versjoner:
  cT %<>% noric::fikse_sykehusnavn(df = .)


  # Tar bort forløp fra før sykehusene ble offisielt med i NORIC
  # (potensielle "tøyseregistreringer")
  cT %<>% noric::fjerne_tulleregistreringer(df = ., var = UndersokDato)


  # Legg til aar, maaned, uke, etc.
  cT %<>% noric::legg_til_tidsvariabler(df = ., var = UndersokDato)

  # Utledete variabler - opptelling av funnkoder i de 20 segmentene (ikke graft)
  cT %<>%
    dplyr::mutate(
      # Opptelling av registrerte funnkoder i segmentene:
      ant_NA = (select(., starts_with("SEGMENT")) %>%
                  is.na() %>%
                  rowSums()),
      ant_0 = (select(., starts_with("SEGMENT")) %>%
                 mutate_all(., list(~ (. %in% 0))) %>%
                 rowSums(., na.rm = TRUE)),
      ant_10 = (select(., starts_with("SEGMENT")) %>%
                  mutate_all(., list(~ (. %in% 10))) %>%
                  rowSums(., na.rm = TRUE)),
      ant_11 = (select(., starts_with("SEGMENT")) %>%
                  mutate_all(., list(~ (. %in% 11))) %>%
                  rowSums(., na.rm = TRUE)),
      ant_12 = (select(., starts_with("SEGMENT")) %>%
                  mutate_all(., list(~ (. %in% 12))) %>%
                  rowSums(., na.rm = TRUE)),
      ant_13 = (select(., starts_with("SEGMENT")) %>%
                  mutate_all(., list(~ (. %in% 13))) %>%
                  rowSums(., na.rm = TRUE)),
      ant_14 = (select(., starts_with("SEGMENT")) %>%
                  mutate_all(., list(~ (. %in% 14))) %>%
                  rowSums(., na.rm = TRUE)),
      ant_15 = (select(., starts_with("SEGMENT")) %>%
                  mutate_all(., list(~ (. %in% 15))) %>%
                  rowSums(., na.rm = TRUE)),
      ant_16 = (select(., starts_with("SEGMENT")) %>%
                  mutate_all(., list(~ (. %in% 16))) %>%
                  rowSums(., na.rm = TRUE)),
      ant_17 = (select(., starts_with("SEGMENT")) %>%
                  mutate_all(., list(~ (. %in% 17))) %>%
                  rowSums(., na.rm = TRUE)),
      ant_18 = (select(., starts_with("SEGMENT")) %>%
                  mutate_all(., list(~ (. %in% 18))) %>%
                  rowSums(., na.rm = TRUE)),
      # Per den nyeste nummereringen, så er obstruktiv stenose (>=50%) kodet
      # med tallene 13,14 og 15:
      ant_obstruktiv = (
        select(., starts_with("SEGMENT")) %>%
          mutate_all(., list(~ (. %in% c(13, 14, 15)))) %>%
          rowSums(., na.rm = TRUE))
    )

  cT
}

