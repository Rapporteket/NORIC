#' getAnDData provides local reg data from AnnenDiagnostikkVar
#'
#' @param registryName String providing the registry name
#' @param singleRow Logical defining if only one row is to be returned. A
#' relevant usecase will be when only description is needed. By default set to
#' FALSE
#' @param ... Optional arguments to be passed to the function
#' @importFrom dplyr filter mutate mutate_all select left_join
#' @importFrom lubridate ymd year month quarter isoweek
#'
#' @return Data frame representing the table AnnenDiagnostikkVar
#' @export
#'

getAnDData <- function(registryName, singleRow = FALSE, ...) {

  dbType <- "mysql"
  query <- "
SELECT *
FROM AnnenDiagnostikkVar
  "

  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query metadata for AnnenDiagnostikk pivot"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for AnnenDiagnostikk pivot"
  }

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }

  anD <- rapbase::loadRegData(registryName, query, dbType)

  fO <- rapbase::loadRegData(registryName,
                             query = "SELECT * FROM ForlopsOversikt")


  # Velger relevante variabler fra fO som skal legges til tabellen:
  fO %<>%
    dplyr::select(
      # Nøkler:
      .data$AvdRESH,
      .data$Sykehusnavn,
      .data$ForlopsID,
      # Variablene som legges til:
      # FodselsDato # Finnes per dags dato i anD fra før
      .data$PasientID,
      .data$Kommune,
      .data$KommuneNr,
      .data$Fylke,
      .data$Fylkenr,
      # ,PasientKjonn # Finnes per dags dato i anD fra før
      .data$PasientAlder,
      .data$ForlopsType1,
      .data$ForlopsType2,
      .data$KobletForlopsID
    )

  # Legger til variabler fra fO til anD:
  anD <- dplyr::left_join(anD, fO, by = c("AvdRESH",
                                          "Sykehusnavn",
                                          "ForlopsID")
  )

  # Gjor datoer om til dato-objekt:
  anD %<>%
    dplyr::mutate_at(
      vars(ends_with("dato", ignore.case = TRUE)), list(ymd)
    )


  # Endre Sykehusnavn til kortere versjoner:
  anD %<>%
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


  # Tar bort forloep fra før sykehusene ble offisielt med i NORIC (potensielle
  # "toeyseregistreringer")
  anD %<>%
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
        # Ullevål
        (.data$AvdRESH == 109880) & (as.Date(.data$ProsedyreDato) >=
                                       "2014-01-01")
      ) | (
        # St Olav
        (.data$AvdRESH == 104284) & (as.Date(.data$ProsedyreDato) >=
                                       "2014-01-01")
      ) | (
        # SSA
        (.data$AvdRESH == 114150) & (as.Date(.data$ProsedyreDato) >=
                                       "2014-01-01")
      ) | (
        # SUS
        (.data$AvdRESH == 105502) & (as.Date(.data$ProsedyreDato) >=
                                       "2014-01-01")
      ) | (
        # Risken
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
        # Bodø
        (.data$AvdRESH == 4210141) & (as.Date(.data$ProsedyreDato) >=
                                        "2020-02-10")
      )
    )

  # Gjøre kategoriske variabler om til factor:
  anD %<>%
    dplyr::mutate(
      ForlopsType2 = factor(.data$ForlopsType2,
                            levels = c(
                              "Akutt",
                              "Subakutt",
                              "Planlagt"
                            ),
                            ordered = TRUE
      ),

      segment = factor(.data$segment,
                       levels = c(
                         "Proximale RCA (1)",
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
                         "Septal (20)"
                       ),
                       ordered = TRUE
      ),

      graft = factor(.data$graft,
                     levels = c(
                       "Arterie",
                       "Vene",
                       "Nei"
                     ),
                     ordered = TRUE
      ),
      metode = factor(.data$metode,
                     levels = c(
                       "iFR",
                       "FFR",
                       "OCT",
                       "IVUS",
                       "CFR",
                       "IMR",
                       "Pd/Pa",
                       "NIRS",
                       "Pa-hyperemi",
                       "Pd-hyperemi"
                     ),
                     ordered = TRUE
      )
    )


  # Utledete variabler:
  anD %<>%
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

  anD

}
