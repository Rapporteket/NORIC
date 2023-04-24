#' Data managment on tables
#'
#' Load data and apply data-managment operations. Tables can be used
#' in \emph{pivot-table} or in monthly reports.
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
#'
#' @name getPrepData
#' @aliases getPrepApData
#' getPrepSoData
#' getPrepAkData
#' getPrepFoData
#' getPrepAnPData
#' getPrepCtData
#' getPrepAkOppfData
#' getPrepAnDData
#' getPrepSsData
#' getPrepMkData
#' getPrepPsData
#' getPrepApLightData
NULL

#' @rdname getPrepData
#' @export
getPrepApData <- function(registryName, fromDate, toDate, singleRow,...){
  
  
  . <- ""
  
  dataListe <- noric::getAp(registryName = registryName,
                            fromDate = fromDate,
                            toDate = toDate,
                            singleRow = singleRow)
  aP <- dataListe$aP
  
  
  # Gjor datoer om til dato-objekt:
  aP %<>%
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("dato", ignore.case = TRUE)),
                     list(lubridate::ymd))
  
  
  # Endre Sykehusnavn til kortere versjoner:
  aP %<>% noric::fikse_sykehusnavn(df = .)
  
  
  if(!singleRow){
    # Tar bort forløp fra før sykehusene ble offisielt med i NORIC
    # (potensielle "tøyseregistreringer")
    aP %<>% noric::fjerne_tulleregistreringer(df = ., var = ProsedyreDato)
  }
  
  # Legg til aar, maaned, uke, etc.
  aP %<>% noric::legg_til_tidsvariabler(df = ., var = ProsedyreDato)
  
  aP
}


#' @rdname getPrepData
#' @export
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
  
  
  if(!singleRow){
    # Tar bort forløp fra før sykehusene ble offisielt med i NORIC (potensielle
    # "tøyseregistreringer")
    sO %<>% noric::fjerne_tulleregistreringer(df = ., var = HovedDato)
  }
  
  # Utledete variabler:
  sO %<>% dplyr::mutate(
    ferdigstilt = as.ordered(ifelse(.data$SkjemaStatus == 1,
                                    yes = "Ferdigstilt",
                                    no = "Ikke ferdigstilt")))
  
  
  
  # Legg til aar, maaned, uke, etc.
  sO %<>% noric::legg_til_tidsvariabler(df = ., var = HovedDato)
  
  sO
}


#' @rdname getPrepData
#' @export
getPrepAkData <- function(registryName, fromDate, toDate, singleRow,...){
  
  
  . <- ""
  
  dataListe <- noric::getAk(registryName = registryName,
                            fromDate = fromDate,
                            toDate = toDate,
                            singleRow = singleRow)
  aK <- dataListe$aK
  
  
  # Gjor datoer om til dato-objekt:
  aK %<>%
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("dato", ignore.case = TRUE)),
                     list(lubridate::ymd))
  
  
  # Endre Sykehusnavn til kortere versjoner:
  aK %<>% noric::fikse_sykehusnavn(df = .)
  
  
  if(!singleRow){
    # Tar bort forløp fra før sykehusene ble offisielt med i NORIC
    # (potensielle "tøyseregistreringer")
    aK %<>% noric::fjerne_tulleregistreringer(df = ., var = ProsedyreDato)
  }
  
  
  # Legg til aar, maaned, uke, etc.
  aK %<>% noric::legg_til_tidsvariabler(df = ., var = ProsedyreDato)
  
  
  # utlede variabler
  aK %<>% dplyr::mutate(
    dager_mellom_prosedyre_og_utskr = as.numeric(difftime(.data$UtskrDato,
                                                          .data$ProsedyreDato,
                                                          units = "days")))
  # Indikator pacemakerbehov
  aK %<>% noric::ki_ak_pacemakerbehov(df_ak = .) 
  
  aK
}

#' @rdname getPrepData
#' @export
getPrepFoData <- function(registryName, fromDate, toDate, singleRow,...){
  
  
  . <- ""
  
  dataListe <- noric::getFo(registryName = registryName,
                            fromDate = fromDate,
                            toDate = toDate,
                            singleRow = singleRow)
  fO <- dataListe$fO
  
  
  # Gjor datoer om til dato-objekt:
  fO %<>%
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("dato", ignore.case = TRUE)),
                     list(lubridate::ymd))
  
  
  # Endre Sykehusnavn til kortere versjoner:
  fO %<>% noric::fikse_sykehusnavn(df = .)
  
  if(!singleRow){
    # Tar bort forløp fra før sykehusene ble offisielt med i NORIC (potensielle
    # "tøyseregistreringer")
    fO %<>% noric::fjerne_tulleregistreringer(df = ., var = HovedDato)
  }
  
  
  # Legg til aar, maaned, uke, etc.
  fO %<>% noric::legg_til_tidsvariabler(df = ., var = HovedDato)
  
  fO
}

#' @rdname getPrepData
#' @export
getPrepAnPData <- function(registryName, fromDate, toDate, singleRow,...){
  
  
  . <- ""
  
  dataListe <- noric::getAnP(registryName = registryName,
                             fromDate = fromDate,
                             toDate = toDate,
                             singleRow = singleRow)
  anP <- dataListe$anP
  
  
  # Gjor datoer om til dato-objekt:
  anP %<>%
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("dato", ignore.case = TRUE)),
                     list(lubridate::ymd))
  
  
  # Endre Sykehusnavn til kortere versjoner:
  anP %<>% noric::fikse_sykehusnavn(df = .)
  
  if(!singleRow){
    # Tar bort forløp fra før sykehusene ble offisielt med i NORIC
    # (potensielle "tøyseregistreringer")
    anP %<>% noric::fjerne_tulleregistreringer(df = ., var = ProsedyreDato)
  }
  
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


#' @rdname getPrepData
#' @export
getPrepCtData <- function(registryName, fromDate, toDate, singleRow,...){
  
  
  . <- ""
  
  dataListe <- noric::getCt(registryName = registryName,
                            fromDate = fromDate,
                            toDate = toDate,
                            singleRow = singleRow)
  cT <- dataListe$cT
  
  
  # Gjor datoer om til dato-objekt:
  cT %<>%
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("dato", ignore.case = TRUE)),
                     list(lubridate::ymd))
  
  
  # Endre Sykehusnavn til kortere versjoner:
  cT %<>% noric::fikse_sykehusnavn(df = .)
  
  
  if(!singleRow){
    # Tar bort forløp fra før sykehusene ble offisielt med i NORIC
    # (potensielle "tøyseregistreringer")
    cT %<>% noric::fjerne_tulleregistreringer(df = ., var = UndersokDato)
  }
  
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

#' @rdname getPrepData
#' @export
getPrepAkOppfData <- function(registryName, fromDate, toDate, singleRow,...){
  
  
  . <- ""
  
  dataListe <- noric::getAkOppf(registryName = registryName,
                                fromDate = fromDate,
                                toDate = toDate,
                                singleRow = singleRow)
  aKoppf <- dataListe$aKoppf
  
  
  # Gjor datoer om til dato-objekt:
  aKoppf %<>%
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("dato", ignore.case = TRUE)),
                     list(lubridate::ymd))
  
  
  # Endre Sykehusnavn til kortere versjoner:
  aKoppf %<>% noric::fikse_sykehusnavn(df = .)
  
  
}


#' @rdname getPrepData
#' @export
getPrepAnDData <- function(registryName, fromDate, toDate, singleRow,...){
  
  
  . <- ""
  
  dataListe <- noric::getAnD(registryName = registryName,
                             fromDate = fromDate,
                             toDate = toDate,
                             singleRow = singleRow)
  anD <- dataListe$anD
  
  
  # Gjor datoer om til dato-objekt:
  anD %<>%
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("dato", ignore.case = TRUE)),
                     list(lubridate::ymd))
  
  
  # Endre Sykehusnavn til kortere versjoner:
  anD %<>% noric::fikse_sykehusnavn(df = .)
  
  
  if(!singleRow){
    # Tar bort forløp fra før sykehusene ble offisielt med i NORIC
    # (potensielle "tøyseregistreringer")
    anD %<>% noric::fjerne_tulleregistreringer(df = ., var = ProsedyreDato)
  }
  
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

#' @rdname getPrepData
#' @export
getPrepSsData <- function(registryName, fromDate, toDate, singleRow,...){
  
  
  . <- ""
  
  dataListe <- noric::getSs(registryName = registryName,
                            fromDate = fromDate,
                            toDate = toDate,
                            singleRow = singleRow)
  sS <- dataListe$sS
  
  
  # Gjor datoer om til dato-objekt:
  sS %<>%
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("dato", ignore.case = TRUE)),
                     list(lubridate::ymd))
  
  
  # Endre Sykehusnavn til kortere versjoner:
  sS %<>% noric::fikse_sykehusnavn(df = .)
  
  
  if(!singleRow){
    # Tar bort forløp fra før sykehusene ble offisielt med i NORIC
    # (potensielle "tøyseregistreringer")
    sS %<>% noric::fjerne_tulleregistreringer(df = ., var = ProsedyreDato)
  }
  
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


#' @rdname getPrepData
#' @export
getPrepMkData <- function(registryName, fromDate, toDate, singleRow,...){
  
  
  . <- ""
  
  dataListe <- noric::getMk(registryName = registryName,
                            fromDate = fromDate,
                            toDate = toDate,
                            singleRow = singleRow)
  mK <- dataListe$mK
  
  
  # Gjor datoer om til dato-objekt:
  mK %<>%
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("dato", ignore.case = TRUE)),
                     list(lubridate::ymd))
  
  
  # Endre Sykehusnavn til kortere versjoner:
  mK %<>% noric::fikse_sykehusnavn(df = .)
  
  
  if(!singleRow){
    # Tar bort forløp fra før sykehusene ble offisielt med i NORIC
    # (potensielle "tøyseregistreringer")
    mK %<>% noric::fjerne_tulleregistreringer(df = ., var = ProsedyreDato)
  }
  
  # Legg til aar, maaned, uke, etc.
  mK %<>% noric::legg_til_tidsvariabler(df = ., var = ProsedyreDato)
  
  
  # utlede variabler
  mK %<>% dplyr::mutate(
    dager_mellom_prosedyre_og_utskr = as.numeric(difftime(.data$UtskrDato,
                                                          .data$ProsedyreDato,
                                                          units = "days")))
  
  # Gjøre kategoriske variabler om til factor:
  # (ikke fullstendig, må legge til mer etter hvert)
  mK %<>%
    dplyr::mutate(
      ForlopsType2 = factor(.data$ForlopsType2,
                            levels = c("Akutt",
                                       "Subakutt",
                                       "Planlagt"),
                            ordered = TRUE),
      
      Frailty = factor(.data$Frailty,
                       levels = c("Robust",
                                  "Intermedi\u00e6r",
                                  "Skr\u00f8pelig",
                                  "Ukjent",
                                  NA),
                       exclude = NULL, # inkluderer NA i levels
                       ordered = TRUE),
      
      Hastegrad = factor(.data$Hastegrad,
                         levels = c("Elektiv",
                                    "Haster",
                                    "Akutt",
                                    "Under p\u00e5g\u00e5ende HLR",
                                    NA),
                         exclude = NULL, # inkluderer NA i levels
                         ordered = TRUE),
      
      PostVenstreVentrikkelFunksjon = factor(
        .data$PostVenstreVentrikkelFunksjon,
        levels = c("Normal",
                   "Lett nedsatt: EF 40 - 49% ",
                   "Moderat nedsatt: EF 30 - 39%",
                   "Betydelig nedsatt: EF 21 - 29%",
                   "Alvorlig nedsatt: EF <= 20%",
                   "Ukjent",
                   NA),
        exclude = NULL, # inkluderer NA i levels
        ordered = TRUE),
      
      PreVenstreVentrikkelFunksjon = factor(
        .data$PreVenstreVentrikkelFunksjon,
        levels = c("Normal",
                   "Lett nedsatt: EF 40 - 49% ",
                   "Moderat nedsatt: EF 30 - 39%",
                   "Betydelig nedsatt: EF 21 - 29%",
                   "Alvorlig nedsatt: EF <= 20%",
                   "Ukjent",
                   NA),
        exclude = NULL,
        ordered = TRUE),
      
      ProsedyreEkko = factor(.data$ProsedyreEkko,
                             levels = c("Nei",
                                        "TEE",
                                        "ICE",
                                        "TTE",
                                        "IVUS",
                                        "Annet",
                                        NA),
                             exclude = NULL, # inkluderer NA i levels
                             ordered = TRUE),
      
      UtskrevetTil = factor(.data$UtskrevetTil,
                            levels = c("Hjem",
                                       "Rehabilitering",
                                       "Annet sykehus",
                                       "Sykehjem",
                                       NA),
                            exclude = NULL, # inkluderer NA i levels
                            ordered = TRUE))
  mK
}


#' @rdname getPrepData
#' @export
getPrepPsData <- function(registryName, fromDate, toDate, singleRow, ...){
  
  
  . <- ""
  
  dataListe <- noric::getPs(registryName = registryName,
                            fromDate = fromDate,
                            toDate = toDate,
                            singleRow = singleRow)
  pS <- dataListe$pS
  
  
  # Gjor datoer om til dato-objekt:
  pS %<>%
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("dato", ignore.case = TRUE)),
                     list(lubridate::ymd))
  
  
  # Endre Sykehusnavn til kortere versjoner:
  pS %<>% noric::fikse_sykehusnavn(df = .)
  
  
  # Utledete variabler:
  pS %<>%
    dplyr::mutate(
      # Div. tidsvariabler:
      # Basert på PasInklDato:
      # Kalenderår:
      aar_pasientinklusjon = as.ordered(lubridate::year(.data$PasInklDato)),
      # Måned:
      # (månedsnr er tosifret; 01, 02, ....)
      maaned_nr_pasientinklusjon =
        as.ordered(sprintf(fmt = "%02d", lubridate::month(.data$PasInklDato))),
      
      maaned_pasientinklusjon =
        as.ordered(paste0(.data$aar_pasientinklusjon, "-",
                          .data$maaned_nr_pasientinklusjon)),
      # Kvartal:
      kvartal_pasientinklusjon = lubridate::quarter(.data$PasInklDato,
                                                    with_year = TRUE),
      kvartal_pasientinklusjon =
        as.ordered(gsub("[[:punct:]]", "-Q", .data$kvartal_pasientinklusjon)),
      # Uketall:
      uke_pasientinklusjon =
        as.ordered(sprintf(fmt = "%02d",
                           lubridate::isoweek(.data$PasInklDato))),
      
      # Variabel med "yyyy-ukenummer" som tar høyde for uketall spredt over to
      # kalenderår:
      aar_uke_pasientinklusjon = ifelse(
        # hvis uke 01 er i desember...
        test = .data$uke_pasientinklusjon == "01" &
          .data$maaned_nr_pasientinklusjon == "12",
        # .. så sier vi at uken tilhører det seneste av de to årene som uke 01
        # er spredt over (uke 01 i desember 2019 blir til 2020-01)
        yes =
          paste0(as.integer(lubridate::year(.data$PasInklDato)) + 1, "-",
                 .data$uke_pasientinklusjon),
        no =
          paste0(.data$aar_pasientinklusjon, "-", .data$uke_pasientinklusjon)
      ),
      aar_uke_pasientinklusjon = ifelse(
        # hvis uke 52 eller 53 er i januar...
        test = .data$uke_pasientinklusjon %in% c("52", "53") &
          .data$maaned_nr_pasientinklusjon == "01",
        # ...sier vi at hele uken tilhører det tidligste av de to årene som uke
        # 52/53 er spredt over (1. januar 2017 som er i uke 52 blir til 2016-52)
        yes = paste0(as.integer(lubridate::year(.data$PasInklDato)) - 1, "-",
                     .data$uke_pasientinklusjon),
        no = .data$aar_uke_pasientinklusjon
      ),
      aar_uke_pasientinklusjon = as.ordered(.data$aar_uke_pasientinklusjon),
      # Basert på StudieStartDato:
      # Kalenderår:
      aar_studiestart = as.ordered(lubridate::year(.data$StudieStartDato)),
      # Måned:
      # (månedsnr er tosifret; 01, 02, ....)
      maaned_nr_studiestart =
        as.ordered(sprintf(fmt = "%02d",
                           lubridate::month(.data$StudieStartDato))),
      maaned_studiestart =
        as.ordered(paste0(.data$aar_studiestart, "-",
                          .data$maaned_nr_studiestart)),
      # Kvartal:
      kvartal_studiestart = lubridate::quarter(.data$StudieStartDato,
                                               with_year = TRUE),
      kvartal_studiestart =
        as.ordered(gsub("[[:punct:]]", "-Q", .data$kvartal_studiestart)),
      # Uketall:
      uke_studiestart =
        as.ordered(sprintf(fmt = "%02d",
                           lubridate::isoweek(.data$StudieStartDato))),
      # Variabel med "yyyy-ukenummer" som tar høyde for uketall spredt over to
      # kalenderår:
      aar_uke_studiestart = ifelse(
        # hvis uke 01 er i desember...
        test = .data$uke_studiestart == "01" &
          .data$maaned_nr_studiestart == "12",
        # .. så sier vi at uken tilhører det seneste av de to årene som uke 01
        # er spredt over (uke 01 i desember 2019 blir til 2020-01)
        yes =
          paste0(as.integer(lubridate::year(.data$StudieStartDato)) + 1, "-",
                 .data$uke_studiestart),
        no = paste0(.data$aar_studiestart, "-", .data$uke_studiestart)
      ),
      aar_uke_studiestart = ifelse(
        # hvis uke 52 eller 53 er i januar...
        test =
          .data$uke_studiestart %in% c("52", "53") &
          .data$maaned_nr_studiestart == "01",
        # ...sier vi at hele uken tilhører det tidligste av de to årene som uke
        # 52/53 er spredt over (1. januar 2017 som er i uke 52 blir til 2016-52)
        yes =
          paste0(as.integer(lubridate::year(.data$StudieStartDato)) - 1, "-",
                 .data$uke_studiestart),
        no = .data$aar_uke_studiestart
      ),
      aar_uke_studiestart = as.ordered(.data$aar_uke_studiestart),
    )
  
  
  
  pS
}

#' @rdname getPrepData
#' @export
getPrepApLightData <- function(registryName, fromDate, toDate, singleRow,...){
  
  
  . <- ""
  
  dataListe <- noric::getApLight(registryName = registryName,
                                 fromDate = fromDate,
                                 toDate = toDate,
                                 singleRow = singleRow)
  ap_light <- dataListe$aP
  sS <- dataListe$sS
  aD <- dataListe$aD
  
  
  
  # Legg til oppholdsID (samme ID for alle prosedyrer tilknyttet samme sykehus-
  # opphold)
  ap_light %<>% noric::utlede_OppholdsID(df = .)
  
  
  # Gjor datoer om til dato-objekt:
  ap_light %<>%
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("dato", ignore.case = TRUE)),
                     list(lubridate::ymd))
  
  
  # Utledete tidsvariabler (aar, maaned, uke osv):
  ap_light %<>% noric::legg_til_tidsvariabler(df = .,
                                              var = .data$ProsedyreDato)
  
  # Endre Sykehusnavn til kortere versjoner:
  ap_light %<>% noric::fikse_sykehusnavn(df = .)
  
  
  
  # Utlede variabler for ferdigstilt eller ikke,
  ap_light %<>%
    noric::utlede_ferdigstilt(df = .,
                              var = .data$SkjemaStatusStart,
                              suffix = "StartSkjema") %>%
    
    noric::utlede_ferdigstilt(df = .,
                              var = .data$SkjemastatusHovedskjema,
                              suffix = "HovedSkjema") %>%
    
    noric::utlede_ferdigstilt(df = .,
                              var = .data$SkjemaStatusUtskrivelse,
                              suffix = "UtskrSkjema") %>%
    
    noric::utlede_ferdigstilt(df = .,
                              var = .data$SkjemaStatusKomplikasjoner,
                              suffix = "KomplikSkjema")
  
  # Utlede aldersklasser
  ap_light %<>% noric::utlede_aldersklasse(df = .,
                                           var = .data$PasientAlder)
  
  # Legger til utledete variabler fra segment Stent til ap_light,
  # Noen er hjelpevariabler som brukes i KI-funksjonene. Disse fjernes
  # før de legges i utforsker.
  ap_light %<>% noric::legg_til_antall_stent(df_ap = .,
                                             df_ss = sS)
  ap_light %<>% noric::legg_til_antall_stent_opphold(df_ap = .)
  ap_light %<>% noric::satt_inn_stent_i_lms(df_ap = .,
                                            df_ss = sS)
  
  
  #  Legge til utledete variabler fra annen Diagnostikk. Hjelpevariabler for
  # trykkmåling. Disse fjernes før tabellen legges i utforsker
  ap_light %<>% noric::legg_til_trykkmaalinger(df_ap = .,
                                               df_ad = aD)
  
  # Legge til kvalitetsindikatorene:
  ap_light %<>% noric::ki_ferdigstilt_komplikasjoner(df_ap = .)
  ap_light %<>% noric::ki_trykkmaaling_utfoert(df_ap = .)
  ap_light %<>% noric::ki_ivus_oct_ved_stenting_lms(df_ap = .)
  ap_light %<>% noric::ki_foreskr_blodfortynnende(df_ap = .)
  ap_light %<>% noric::ki_foreskr_kolesterolsenkende(df_ap = .)
  
  ap_light %<>%
    noric::legg_til_ventetid_nstemi_timer(df_ap = .) %>%
    noric::ki_nstemi_utredet_innen24t(df_ap = .) %>%
    noric::ki_nstemi_utredet_innen72t(df_ap = .)
  ap_light %<>%
    noric::legg_til_ventetid_stemi_min(df_ap = .) %>%
    noric::ki_stemi_pci_innen120min(df_ap = .)
  
  
  
  # Legg til liggedogn
  ap_light %<>% noric::legg_til_liggedogn(df_ap = .)
  
  
  # Fjerne noen variabler.
  #  Se variablliste fra NORIC
  ap_light %<>%
    dplyr::select(
      # Foretrekker de utledete "ferdigstilt.. " variablene:
      - .data$SkjemaStatusStart,
      - .data$SkjemastatusHovedskjema,
      - .data$SkjemaStatusUtskrivelse,
      - .data$SkjemaStatusKomplikasjoner,
      
      # Overflødig, fordi tilhørende kont. verdi er NA:
      - tidyselect::contains("Ukjent"),
      
      # Ikke i bruk
      - .data$PasientRegDato,
      - .data$Studie,
      
      # Dobbelt opp av disse, fjerne minst komplette/feil (sept 2021):
      -.data$BesUtlEKGDato,
      -.data$BesUtlEKGTid,
      -.data$KillipKlasseAnkomst,
      -.data$KardiogentSjokk,
      -.data$Kreatinin,
      
      # Fjerne alle init-medikamenter:
      -.data$InitASA,
      -.data$InitAntikoagulantia,
      -.data$InitAndrePlatehemmere,
      -.data$InitStatiner,
      -.data$InitNSAID,
      -.data$InitACEHemmere,
      -.data$InitA2Blokkere,
      -.data$InitBetaBlokkere,
      -.data$InitCaHemmere,
      -.data$InitDiabetesPrOral,
      -.data$InitDigitalis,
      -.data$InitDiuretika,
      -.data$InitAldosteronantagonist,
      -.data$InitOvrigLipid,
      -.data$InitNitroglycerin,
      
      # Fjerne alle init blodprøver
      - .data$Infarktmarkoer,
      - .data$InfarktMarkoerMax,
      - .data$Kolesterol,
      - .data$Triglycerider,
      - .data$HDL,
      - .data$MaaltLDL,
      - .data$SGlukose,
      - .data$HbA1c,
      - .data$Kreatinin,
      - .data$CRP,
      - .data$Hemoglobin,
      
      # Fjerne komplikasjoner
      - tidyselect::contains("AvdKomp"),
      - tidyselect::contains("LabKomp"),
      
      
      # Mediakmenter ved utskrivelse:
      - .data$NSAID,
      - .data$ACEHemmere,
      - .data$A2Blokkere,
      - .data$Betablokkere,
      - .data$CaBlokkere,
      - .data$DiabetesBehandlingInsulin,
      - .data$DiabetesBehandlingPerOral,
      - .data$Digitalis,
      - .data$Diuretika,
      - .data$Aldosteronantagonister,
      - .data$NitroglycerinLangtid,
      
      - .data$TroponinVerdiFor,
      - .data$TroponinMetFor,
      - .data$TroponinVerdiEtter,
      - .data$TroponinMetEtter,
      - .data$CKMBFor,
      - .data$CKMBEtter,
      
      # Andre variabler utskrivelse
      - .data$InfarktType,
      - .data$InfarktSubklasse,
      - .data$UtskrDiagnoser,
      - .data$AnnenAlvorligSykdom)
  
  # Fjerne utledete hjelpevariabler
  ap_light %<>%
    dplyr::select(- .data$antall_stent_under_opphold,
                  - .data$satt_inn_stent_i_LMS)
  
  
  # Må fjerne disse etter oppdatering av innreg-prod.. De vil komme av seg selv
  # - .data$IMR,
  # - .data$PdPa,
  # - .data$Pa,
  # - .data$Pd)
  
  # Gjøre kategoriske variabler om til factor:
  ap_light %<>%
    dplyr::mutate(
      Hastegrad = factor(.data$Hastegrad,
                         levels = c("Akutt",
                                    "Subakutt",
                                    "Planlagt"),
                         ordered = TRUE))
  
  if(!singleRow){
    # Tar bort forløp fra før sykehusene ble offisielt med i NORIC
    # (potensielle "tøyseregistreringer")
    ap_light %<>% noric::fjerne_tulleregistreringer(df = ., var = ProsedyreDato)
  }
  
  ap_light
}





#' @rdname getPrepData
#' @export
getPrepTaviPromData <- function(registryName, fromDate, toDate, singleRow,...){
  
  
  . <- ""
  
  dataListe <- noric::getTaviProm(registryName = registryName,
                                  fromDate = fromDate,
                                  toDate = toDate,
                                  singleRow = singleRow)
  tP <- dataListe$taviProm
  aK <- dataListe$aK
  
  nyeste_eprom_bestilling <- lubridate::date(max(tP$ProsedyreDato)) 
  
  # KOBLE med variabler fra AK
  tavi <- dplyr::left_join(
    aK, 
    tP %>%
      dplyr::select(-ProsedyreDato, -FnrType) %>% 
      dplyr::mutate(
        eprom_bestilt = "ja"), 
    by = c("ForlopsID", "AvdRESH", "PasientID")) 
   
    
  # Datagrunnlag for ePROM
    tavi %<>% 
     dplyr::mutate(
      eprom_bestilt = dplyr::case_when(
        
        ProsedyreDato > nyeste_eprom_bestilling ~ 
          "nei, registreringen er for ny", 
  
        ProsedyreDato < as.Date("2022-12-19", format = "%Y-%m-%d") ~ 
          "nei, før innføring av prom",
        
        is.na(eprom_bestilt) ~
          "nei",
        
       !is.na(eprom_bestilt) ~ 
         "ja")
      )
      
    tavi %<>% 
      dplyr::mutate(
        dg_prosedyre_til_dod = dplyr::if_else(.data$Avdod == "Ja", 
                                              as.numeric(difftime(Dodsdato, 
                                                       ProsedyreDato, 
                                                       units = "days")), 
                                              NA_real_))
    
      # Endre Sykehusnavn til kortere versjoner:
      tavi %<>% noric::fikse_sykehusnavn(df = .)
      
    
      
      # LEgg til listestekst
      
      tavi %<>%
        noric::legg_til_taviStatus()
      
      # Fikse rekkeflge
      tavi %>% 
        dplyr::select(.data$AvdRESH,
                      .data$Sykehusnavn,
                      .data$PasientID, 
                      .data$ForlopsID, 
                      .data$FnrType, 
                      .data$PasientAlder, 
                      .data$PasientKjonn,
                      .data$Avdod, 
                      .data$Dodsdato, 
                      .data$TypeKlaffeprotese, 
                      .data$UtskrevetTil, 
                      .data$Prosedyre, 
                      .data$ScreeningBeslutning, 
                      .data$ProsedyreDato, 
                      .data$dg_prosedyre_til_dod, 
                      .data$eprom_bestilt, 
                      .data$ePromStatus,
                      .data$ePromStatus_tekst, 
                      .data$ePromBestillingsdato:.data$ePromUtloeptDato, 
                      .data$Registreringstype, 
                      .data$rose01:.data$premStatus) %>% 
        # Legg til aar, maaned, uke, etc.
        noric::legg_til_tidsvariabler(df = ., var = ProsedyreDato)
      
}

