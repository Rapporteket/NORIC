#' getPSData provides local or national reg data from PasienterStudier
#'
#' @param registryName String providing the registry name
#' @param singleRow Logical defining if only one row is to be returned. A
#' relevant usecase will be when only description is needed. By default set to
#' FALSE
#' @param ... Optional arguments to be passed to the function
#' @importFrom dplyr filter mutate mutate_all select recode left_join
#' @importFrom lubridate ymd year month quarter isoweek
#'
#' @return Data frame representing the table PasienterStudier
#' @export
#'

getPSData <- function(registryName, singleRow = FALSE, ...) {

  dbType <- "mysql"
  query <- "
SELECT
  *
FROM
  PasienterStudier
"

  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query metadata for PasienterStudier pivot"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for PasienterStudier pivot"
  }

  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]], msg = msg)
  }

  pS <- rapbase::loadRegData(registryName, query, dbType)

  fO <- rapbase::loadRegData(registryName,
                             query = "SELECT * FROM ForlopsOversikt")


  # Velger relevante variabler fra fO som skal legges til tabellen:
  fO %<>%
    dplyr::select(
      # Nøkler:
      .data$AvdRESH,
      .data$PasientID,
      # Variablene som legges til:
      .data$Sykehusnavn,
      .data$FodselsDato,
      .data$Kommune,
      .data$KommuneNr,
      .data$Fylke,
      .data$Fylkenr,
      .data$PasientKjonn,
      .data$PasientAlder
    )


  pS <- dplyr::left_join(pS, fO, by = c("PasientID", "AvdRESH"),
                         suffix = c("", ".fO"))



  # Gjor datoer om til dato-objekt:
  pS %<>%
    dplyr::mutate_at(
      vars(ends_with("dato", ignore.case = TRUE)), list(lubridate::ymd)
    )


  # Endre Sykehusnavn til kortere versjoner:
  pS %<>%
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
        .data$Sykehusnavn
      )
    )


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
