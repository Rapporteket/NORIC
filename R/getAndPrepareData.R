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
#' @aliases getPrepApData
#' getPrepSOData
#' getPrepAkData
#' getPrepFoData
#' getPrepAnPData
#' getPrepCtData
#' getPrepAkOppfData
#' getPrepAnDData
#' getPrepSsData
#' NULL

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

getPrepAkOppfData <- function(registryName, fromDate, toDate, singleRow,...){


  . <- ""

  dataListe <- noric::getAkOppf(registryName = registryName,
                                fromDate = fromDate,
                                toDate = toDate,
                                singleRow = singleRow)
  aKoppf <- dataListe$aKoppf


  # Gjor datoer om til dato-objekt:
  aKoppf %<>%
    dplyr::mutate_at(vars(ends_with("dato", ignore.case = TRUE)),
                     list(ymd))


  # Endre Sykehusnavn til kortere versjoner:
  aKoppf %<>% noric::fikse_sykehusnavn(df = .)


}


getPrepAnDData <- function(registryName, fromDate, toDate, singleRow,...){


  . <- ""

  dataListe <- noric::getAnD(registryName = registryName,
                             fromDate = fromDate,
                             toDate = toDate,
                             singleRow = singleRow)
  anD <- dataListe$anD


  # Gjor datoer om til dato-objekt:
  anD %<>%
    dplyr::mutate_at(vars(ends_with("dato", ignore.case = TRUE)),
                     list(ymd))


  # Endre Sykehusnavn til kortere versjoner:
  anD %<>% noric::fikse_sykehusnavn(df = .)


  # Tar bort forløp fra før sykehusene ble offisielt med i NORIC
  # (potensielle "tøyseregistreringer")
  anD %<>% noric::fjerne_tulleregistreringer(df = ., var = ProsedyreDato)


  # Legg til aar, maaned, uke, etc.
  anD %<>% noric::legg_til_tidsvariabler(df = ., var = ProsedyreDato)

  # Gjøre kategoriske variabler om til factor:
  anD %<>%
    dplyr::mutate(
      ForlopsType2 = factor(.data$ForlopsType2,
                            levels = c("Akutt",
                                       "Subakutt",
                                       "Planlagt"),
                            ordered = TRUE),

      segment = factor(.data$segment,
                       levels = c("Proximale RCA (1)",
                                  "Midtre RCA (2)",
                                  "Distale RCA (3)",
                                  "PDA/RPD (4)",
                                  "Ve hovedstamme (5)",
                                  "Proximale LAD (6)",
                                  "Midtre LAD (7)",
                                  "Distale LAD (8)",
                                  "F\u00f8rste diagonal (9)",
                                  "Andre diagonal (10)",
                                  "Proximale LCx (11)",
                                  "F\u00f8rste obtusa marginal (12)",
                                  "Andre obtusa marginal (13)",
                                  "Distale LCx (14)",
                                  "LPD (15)",
                                  "PLA fra venstre (16)",
                                  "Intermedi\u00e6r (17)",
                                  "PLA (18)",
                                  "H\u00f8yrekammergren (19)",
                                  "Septal (20)"),
                       ordered = TRUE),

      graft = factor(.data$graft,
                     levels = c("Arterie",
                                "Vene",
                                "Nei"),
                     ordered = TRUE),
      metode = factor(.data$metode,
                      levels = c("iFR",
                                 "FFR",
                                 "OCT",
                                 "IVUS",
                                 "CFR",
                                 "IMR",
                                 "Pd/Pa",
                                 "NIRS",
                                 "Pa-hyperemi",
                                 "Pd-hyperemi"),
                      ordered = TRUE))

  anD
}

getPrepSsData <- function(registryName, fromDate, toDate, singleRow,...){


  . <- ""

  dataListe <- noric::getSs(registryName = registryName,
                            fromDate = fromDate,
                            toDate = toDate,
                            singleRow = singleRow)
  sS <- dataListe$sS


  # Gjor datoer om til dato-objekt:
  sS %<>%
    dplyr::mutate_at(vars(ends_with("dato", ignore.case = TRUE)),
                     list(ymd))


  # Endre Sykehusnavn til kortere versjoner:
  sS %<>% noric::fikse_sykehusnavn(df = .)


  # Tar bort forløp fra før sykehusene ble offisielt med i NORIC
  # (potensielle "tøyseregistreringer")
  sS %<>% noric::fjerne_tulleregistreringer(df = ., var = ProsedyreDato)


  # Legg til aar, maaned, uke, etc.
  sS %<>% noric::legg_til_tidsvariabler(df = ., var = ProsedyreDato)

  # Gjøre kategoriske variabler om til factor:
  # (ikke fullstendig, må legge til mer etter hvert)
  sS %<>%
    dplyr::mutate(
      Graft = factor(.data$Graft,
                     levels = c("Nei",
                                "Arteriell",
                                "Vene"),
                     exclude = NULL, # inkluderer NA i levels
                     ordered = TRUE),

      Stenoseklasse = factor(.data$Stenoseklasse,
                             levels = c("A",
                                        "B1",
                                        "B1 Bifurkasjon",
                                        "B2",
                                        "B2 Bifurkasjon",
                                        "C",
                                        "C Bifurkasjon",
                                        "Annet"),
                             ordered = TRUE),

      StenoseType = factor(.data$StenoseType,
                           levels = c("DeNovo",
                                      "In-stent restenose",
                                      "Stenttrombose",
                                      "Andre restenoser"),
                           ordered = TRUE))
  # Utledet variabel:
  # ant_stent_ila_forlop = antall stenter satt inn ila ett forløp
  sS %<>%
    dplyr::group_by(.data$AvdRESH, .data$ForlopsID) %>%
    dplyr::mutate(antall_stent_ila_forlop = sum(!is.na(.data$StentType))) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$AvdRESH, .data$ForlopsID)

  sS
}


