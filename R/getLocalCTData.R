#' getLocalCTData provides local reg data from CTAngioVar
#'
#' @param registryName String providing the registry name
#' @param singleRow Logical defining if only one row is to be returned. A
#' relevant usecase will be when only description is needed. By default set to
#' FALSE
#' @param ... Optional arguments to be passed to the function
#' @return Data frame representing the table CTAngioVar
#' @importFrom dplyr filter mutate mutate_all select left_join
#' @importFrom tidyselect starts_with
#' @importFrom lubridate ymd year month quarter isoweek
#' @export

getLocalCTData <- function(registryName, singleRow = FALSE, ...) {

  # declare dot
  . <- ""

  dbType <- "mysql"
  query <- "
SELECT *
FROM CTAngioVar
  "

  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query metadata for CTAngio pivot"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for CTAngio pivot"
  }

  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]], msg = msg)
  }

  cT <- rapbase::loadRegData(registryName, query, dbType)

  fO <- rapbase::loadRegData(registryName,
                             query = "SELECT * FROM ForlopsOversikt")

  # Velger relevante variabler fra fO som skal legges til tabellen:
  fO %<>%
    dplyr::select(
      # Nøkler:
      .data$AvdRESH,
      .data$ForlopsID,
      # Variablene som legges til:
      .data$Sykehusnavn,
      .data$Kommune,
      .data$KommuneNr,
      .data$Fylke,
      .data$Fylkenr,
      .data$PasientKjonn,
      .data$PasientAlder,
      .data$ForlopsType1,
      .data$ForlopsType2
    )

  cT <- dplyr::left_join(cT, fO, by = c("ForlopsID",
                                        "AvdRESH"),
                         suffix = c("", ".fO"))


  # Gjor datoer om til dato-objekt:
  cT %<>%
    dplyr::mutate_at(
      vars(ends_with("dato", ignore.case = TRUE)), list(ymd)
    )


  # Endre Sykehusnavn til kortere versjoner:
  cT %<>%
    dplyr::mutate(
      Sykehusnavn = ifelse(
        .data$Sykehusnavn == "Haukeland",
        "HUS",
        .data$Sykehusnavn),
      Sykehusnavn = ifelse(
        .data$Sykehusnavn %in% c("St.Olav", "St. Olav"),
        "St.Olavs",
        .data$Sykehusnavn),
      Sykehusnavn = ifelse(
        .data$Sykehusnavn == "Akershus universitetssykehus HF",
        "Ahus",
        .data$Sykehusnavn)
    )


  # Tar bort forløp fra før sykehusene ble offisielt med i NORIC (potensielle
  # "tøyseregistreringer")
  cT %<>%
    dplyr::filter(
      (
        # HUS
        (.data$AvdRESH == 102966) & (as.Date(.data$UndersokDato) >=
                                       "2013-01-01")
      ) | (
        # UNN
        (.data$AvdRESH == 101619) & (as.Date(.data$UndersokDato) >=
                                       "2013-05-01")
      ) | (
        # Ullevaal
        (.data$AvdRESH == 109880) & (as.Date(.data$UndersokDato) >=
                                       "2014-01-01")
      ) | (
        # St Olav
        (.data$AvdRESH == 104284) & (as.Date(.data$UndersokDato) >=
                                       "2014-01-01")
      ) | (
        # SSA
        (.data$AvdRESH == 114150) & (as.Date(.data$UndersokDato) >=
                                       "2014-01-01")
      ) | (
        # SUS
        (.data$AvdRESH == 105502) & (as.Date(.data$UndersokDato) >=
                                       "2014-01-01")
      ) | (
        # Riksen
        (.data$AvdRESH == 700422) & (as.Date(.data$UndersokDato) >=
                                       "2015-01-01")
      ) | (
        # LHL Gardermoen
        (.data$AvdRESH == 106944) & (as.Date(.data$UndersokDato) >=
                                       "2015-01-01")
      ) | (
        # Ahus
        (.data$AvdRESH == 108141) & (as.Date(.data$UndersokDato) >=
                                       "2016-01-01")
      ) | (
        # Bodoe
        (.data$AvdRESH == 4210141) & (as.Date(.data$UndersokDato) >=
                                        "2020-02-10")
      )
    )


  # Utledete variabler:
  cT %<>%
    dplyr::mutate(
      # Div. tidsvariabler:
      #
      # Kalenderår for UndersokDato:
      aar = as.ordered(lubridate::year(.data$UndersokDato)),
      # Måned:
      # (månedsnr er tosifret; 01, 02, ....)
      maaned_nr = as.ordered(sprintf(fmt = "%02d",
                                     lubridate::month(.data$UndersokDato))),
      maaned = as.ordered(paste0(.data$aar, "-", .data$maaned_nr)),
      # Kvartal:
      kvartal = lubridate::quarter(.data$UndersokDato, with_year = TRUE),
      kvartal = as.ordered(gsub("[[:punct:]]", "-Q", .data$kvartal)),
      # Uketall:
      uke = as.ordered(sprintf(fmt = "%02d",
                               lubridate::isoweek(.data$UndersokDato))),
      # Variabel med "yyyy-ukenummer" som tar høyde for uketall spredt over to
      # kalenderår:
      aar_uke = ifelse(
        # hvis uke 01 er i desember...
        test = .data$uke == "01" & .data$maaned_nr == "12",
        # .. så sier vi at uken tilhører det seneste av de to årene som uke 01
        # er spredt over (uke 01 i desember 2019 blir til 2020-01)
        yes = paste0(as.integer(lubridate::year(.data$UndersokDato)) + 1, "-",
                     .data$uke),
        no = paste0(.data$aar, "-", .data$uke)
      ),
      aar_uke = ifelse(
        # hvis uke 52 eller 53 er i januar...
        test = .data$uke %in% c("52", "53") & .data$maaned_nr == "01",
        # ...sier vi at hele uken tilhører det tidligste av de to årene som uke
        # 52/53 er spredt over (1. januar 2017 som er i uke 52 blir til 2016-52)
        yes = paste0(as.integer(lubridate::year(.data$UndersokDato)) - 1, "-",
                     .data$uke),
        no = .data$aar_uke
      ),
      aar_uke = as.ordered(.data$aar_uke)
    )

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
