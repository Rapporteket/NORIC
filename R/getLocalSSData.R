#' getLocalSSData provides local reg data from SegmentStent
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
#' @importFrom dplyr arrange count filter group_by left_join mutate mutate_at
#' select ungroup
#' @importFrom lubridate isoweek month quarter year ymd
#' @return Data frame representing the table SegmentStent
#' @export
#'

getLocalSSData <- function(registryName, fromDate = NULL, toDate = NULL,
                           singleRow = FALSE, ...) {

  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- noric::getLatestEntry(registryName)
  }

  dbType <- "mysql"
  query <- paste0("
SELECT
  *
FROM
  SegmentStent
WHERE
  ProsedyreDato >= '", fromDate, "' AND ProsedyreDato <= '", toDate, "'"
  )

  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query metadata for SegmentStent pivot"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for SegmentStent pivot"
  }

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }

  sS <- rapbase::loadRegData(registryName, query, dbType)

  query <- paste("
SELECT
  *
FROM
  ForlopsOversikt
WHERE
  HovedDato >= '", fromDate, "' AND HovedDato <= '", toDate, "'"
  )

  fO <- rapbase::loadRegData(registryName, query)

  # Velger relevante variabler fra fO som skal legges til tabellen:
  fO %<>%
    dplyr::select(
      # Nøkler:
      .data$AvdRESH,
      .data$ForlopsID,
      .data$Sykehusnavn,
      # Variablene som legges til:
      .data$PasientID,
      .data$Kommune,
      .data$KommuneNr,
      .data$Fylke,
      .data$Fylkenr,
      .data$PasientAlder,
      .data$ForlopsType1,
      .data$ForlopsType2,
      .data$KobletForlopsID
    )

  sS <- dplyr::left_join(sS, fO, by = c("ForlopsID", "AvdRESH", "Sykehusnavn"),
                         suffix = c("", ".fO"))

  # Gjor datoer om til dato-objekt:
  sS %<>%
    dplyr::mutate_at(
      vars(ends_with("dato", ignore.case = TRUE)), list(lubridate::ymd)
    )


  # Endre Sykehusnavn til kortere versjoner:
  sS %<>%
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
  sS %<>%
    dplyr::filter(
      (
        # HUS
        (.data$AvdRESH == 102966) & (as.Date(.data$ProsedyreDato) >=
                                       "2013-01-01")
      ) | (
        # UNN
        (.data$AvdRESH == 101619) & (as.Date(.data$ProsedyreDato) >=
                                       "2013-05-01")
      ) | (
        # Ullevaal
        (.data$AvdRESH == 109880) & (as.Date(.data$ProsedyreDato) >=
                                       "2014-01-01")
      ) | (
        # St Olav
        (.data$AvdRESH == 104284) & (as.Date(.data$ProsedyreDato) >=
                                       "2014-01-01")
      ) | (
        #SSA
        (.data$AvdRESH == 114150) & (as.Date(.data$ProsedyreDato) >=
                                       "2014-01-01")
      ) | (
        #SUS
        (.data$AvdRESH == 105502) & (as.Date(.data$ProsedyreDato) >=
                                       "2014-01-01")
      ) | (
        # Riksen
        (.data$AvdRESH == 700422) & (as.Date(.data$ProsedyreDato) >=
                                       "2015-01-01")
      ) | (
        # LHL Gardermoen
        (.data$AvdRESH == 106944) & (as.Date(.data$ProsedyreDato) >=
                                       "2015-01-01")
      ) | (
        # Ahus
        (.data$AvdRESH == 108141) & (as.Date(.data$ProsedyreDato) >=
                                       "2016-01-01")
      ) | (
        # Bodoe
        (.data$AvdRESH == 4210141) & (as.Date(.data$ProsedyreDato) >=
                                        "2020-02-10")
      )
    )


  # Gjøre kategoriske variabler om til factor:
  # (ikke fullstendig, må legge til mer etter hvert)
  sS %<>%
    dplyr::mutate(
      Graft = factor(.data$Graft,
                     levels = c(
                       "Nei",
                       "Arteriell",
                       "Vene"
                     ),
                     exclude = NULL, # inkluderer NA i levels
                     ordered = TRUE
      ),

      Stenoseklasse = factor(.data$Stenoseklasse,
                             levels = c(
                               "A",
                               "B1",
                               "B1 Bifurkasjon",
                               "B2",
                               "B2 Bifurkasjon",
                               "C",
                               "C Bifurkasjon",
                               "Annet"
                             ),
                             ordered = TRUE
      ),

      StenoseType = factor(.data$StenoseType,
                           levels = c(
                             "DeNovo",
                             "In-stent restenose",
                             "Stenttrombose",
                             "Andre restenoser"
                           ),
                           ordered = TRUE
      )
    )


  # Utledete variabler:
  sS %<>%
    dplyr::mutate(
      # Div. tidsvariabler:
      #
      # Kalenderår for ProsedyreDato:
      aar = as.ordered(lubridate::year(.data$ProsedyreDato)),
      # Måned:
      # (månedsnr er tosifret; 01, 02, ....)
      maaned_nr = as.ordered(sprintf(fmt = "%02d",
                                     lubridate::month(.data$ProsedyreDato))),
      maaned = as.ordered(paste0(.data$aar, "-", .data$maaned_nr)),
      # Kvartal:
      kvartal = lubridate::quarter(.data$ProsedyreDato, with_year = TRUE),
      kvartal = as.ordered(gsub("[[:punct:]]", "-Q", .data$kvartal)),
      # Uketall:
      uke = as.ordered(sprintf(fmt = "%02d",
                               lubridate::isoweek(.data$ProsedyreDato))),
      # Variabel med "yyyy-ukenummer" som tar høyde for uketall spredt over to
      # kalenderår:
      aar_uke = ifelse(
        # hvis uke 01 er i desember...
        test = .data$uke == "01" & .data$maaned_nr == "12",
        # .. så sier vi at uken tilhører det seneste av de to årene som uke 01
        # er spredt over (uke 01 i desember 2019 blir til 2020-01)
        yes = paste0(as.integer(lubridate::year(.data$ProsedyreDato)) + 1, "-",
                     .data$uke),
        no = paste0(.data$aar, "-", .data$uke)
      ),
      aar_uke = ifelse(
        # hvis uke 52 eller 53 er i januar...
        test = .data$uke %in% c("52", "53") & .data$maaned_nr == "01",
        # ...sier vi at hele uken tilhører det tidligste av de to årene som uke
        # 52/53 er spredt over (1. januar 2017 som er i uke 52 blir til 2016-52)
        yes = paste0(as.integer(lubridate::year(.data$ProsedyreDato)) - 1, "-",
                     .data$uke),
        no = .data$aar_uke
      ),
      aar_uke = as.ordered(.data$aar_uke)
    )


  # Utledet variabel:
  # ant_stent_ila_forlop = antall stenter satt inn ila ett forløp

  sS %<>%
    dplyr::group_by(.data$Sykehusnavn, .data$ForlopsID) %>%
    dplyr::mutate(antall_stent_ila_forlop = sum(!is.na(.data$StentType))) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$Sykehusnavn, .data$ForlopsID)

  sS

}
