#' getAPDataLight provides local reg data from AngioPCIVar + useful generated
#' variables
#'
#' @param registryName String providing the registry name
#' @param singleRow Logical defining if only one row is to be returned. A
#' relevant usecase will be when only description is needed. By default set to
#' FALSE
#' @param ... Optional arguments to be passed to the function
#' @importFrom dplyr filter mutate mutate_all select left_join
#'
#' @return Data frame representing a simplified version of angio PCI-data
#' @export
#'

getAPDataLight <- function(registryName, singleRow = FALSE, ...) {

  dbType <- "mysql"
  query <- "
SELECT *
FROM AngioPCIVar
  "

  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query metadata for AngioPCI pivot"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for AngioPCI pivot"
  }

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }

  aP_light <- rapbase::loadRegData(registryName, query, dbType)

  fO <- rapbase::loadRegData(registryName,
                             query = "SELECT * FROM ForlopsOversikt")
  sS <- rapbase::loadRegData(registryName,
                             query = "SELECT * FROM SegmentStent")


  # Velger relevante variabler fra fO som skal legges til tabellen:
  fO %<>%
    dplyr::select(
      # Nøkler:
      .data$AvdRESH,
      .data$PasientID,
      .data$ForlopsID,

      # Variablene som legges til:
      .data$Kommune,
      .data$KommuneNr,
      .data$Fylke,
      .data$Fylkenr,
      .data$PasientAlder,
      .data$KobletForlopsID)

  # Legger til variabler fra fO til aP:
  ap_light <- dplyr::left_join(aP, fO, by = c("AvdRESH",
                                              "PasientID",
                                              "ForlopsID"))



  # Legger til utledete variabler fra segment Stent til ap_light
  ap_light %<>% legg_til_antall_stent(ap = ., ss = sS)


   # Gjor datoer om til dato-objekt:
  ap_light %<>%
    dplyr::mutate_at(
      vars(ends_with("dato", ignore.case = TRUE)), list(ymd))


  # Endre Sykehusnavn til kortere versjoner:
  ap_light %<>% noric::fikse_sykehusnavn()

  # Tar bort forløp fra før sykehusene ble offisielt med i NORIC (potensielle
  # "tøyseregistreringer")
  ap_light %<>% noric::fjerne_tulleregistreringer(., var = .data$ProsedyreData)


  # Gjøre kategoriske variabler om til factor:
  # (ikke fullstendig, må legge til mer etter hvert)
  ap_light %<>%
    dplyr::mutate(
      Hastegrad = factor(.data$Hastegrad,
                         levels = c("Akutt",
                                    "Subakutt",
                                    "Planlagt"),
                         ordered = TRUE))


  # Utledete tidsvariabler (aar, maaned, uke osv):
  ap_light %<>% legg_til_tidsvariabler(., var = .data$ProsedyreDato)


  # Utlede aldersklasser
  ap_light %<>% utlede_aldersklasse(., var = .data$PasientAlder) %>%
    relocate(.data$aldersklasse,
             .after = .data$PasientAlder)


  # Utlede variabler for ferdigstilt eller ikke
  ap_light %<>%
    utlede_ferdigstilt(., var = SkjemaStatusStart,
                       suffix = "StartSkjema") %>%
    utlede_ferdigstilt(., var = SkjemastatusHovedskjema,
                       suffix = "HovedSkjema") %>%
    utlede_ferdigstilt(., var = SkjemaStatusUtskrivelse,
                       suffix = "UtskrSkjema") %>%
    utlede_ferdigstilt(., var = SkjemaStatusKomplikasjoner,
                       suffix = "KomplikSkjema")


  # Legge til utledete variabler fra segment stent
}
