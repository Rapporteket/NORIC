#' getMKData provides local or national reg data from MitralklaffVar
#'
#' @param registryName String providing the registry name
#' @param singleRow Logical defining if only one row is to be returned. A
#' relevant usecase will be when only description is needed. By default set to
#' FALSE
#' @param ... Optional arguments to be passed to the function
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter mutate mutate_all select recode left_join
#' @importFrom lubridate ymd year month quarter isoweek
#'
#' @return Data frame representing the table MitralklaffVar
#' @export
#'

getMKData <- function(registryName, singleRow = FALSE, ...) {

  dbType <- "mysql"
  query <- "
SELECT
  *
FROM
  MitralklaffVar
"

  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query metadata for MitralklaffVar pivot"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for MitralklaffVar pivot"
  }

  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]], msg = msg)
  }

  mK <- rapbase::LoadRegData(registryName, query, dbType)

  fO <- rapbase::LoadRegData(registryName,
                             query = "SELECT * FROM ForlopsOversikt")


  # Velger relevante variabler fra fO som skal legges til tabellen:
  fO %<>%
    select(
      # Nøkler:
      .data$AvdRESH,
      .data$ForlopsID,
      # Variablene som legges til:
      .data$Sykehusnavn,
      .data$PasientID,
      .data$KommuneNr,
      .data$Kommune,
      .data$Fylke,
      .data$Fylkenr,
      .data$PasientKjonn,
      .data$PasientAlder,
      .data$BasisRegStatus,
      .data$ForlopsType1,
      .data$ForlopsType2,
      .data$KobletForlopsID,
      .data$FodselsDato
    )


  mK <- dplyr::left_join(mK, fO, by = c("ForlopsID", "AvdRESH"),
                  suffix = c("", ".FO"))


  # Gjor datoer om til dato-objekt:
  mK %<>%
    dplyr::mutate_at(
      vars(ends_with("dato", ignore.case = TRUE)), list(ymd)
    )


  # Endre Sykehusnavn til kortere versjoner:
  mK %<>%
    dplyr::mutate(
      Sykehusnavn = ifelse(.data$Sykehusnavn == "Haukeland", "HUS",
                            .data$Sykehusnavn),
      Sykehusnavn = ifelse(
        .data$Sykehusnavn %in% c("St.Olav", "St. Olav"), "St.Olavs",
        .data$Sykehusnavn),
      Sykehusnavn = ifelse(
        .data$Sykehusnavn == "Akershus universitetssykehus HF", "Ahus",
        .data$Sykehusnavn)
    )


  # Per dags dato tar vi ikke bort forløp som har registrert ProsedyreDato fra
  # før TAVI-registreringene i NORIC startet offisielt (1/1/2017).

  # (Fra 7/6-2019 har vi at  i noen tilfeller behøver eldre prosedyrer, så
  # vi filtrerer ikke etter dato i det hele tatt nå til å begynne med.
  # Kan ev. på sikt legge til en parameter hvor man kan spesifisere om
  # man vil kun ha prosedyrer fra det "offisielle" tidsrommet.)




  # Gjøre kategoriske variabler om til factor:
  # (ikke fullstendig, må legge til mer etter hvert)
  mK %<>%
    dplyr::mutate(
      ForlopsType2 = factor(.data$ForlopsType2,
                              levels = c(
                                "Akutt",
                                "Subakutt",
                                "Planlagt"
                              ),
                              ordered = TRUE),
      Frailty = factor(.data$Frailty,
                        levels = c(
                          "Robust",
                          "Intermediær",
                          "Skrøpelig",
                          "Ukjent",
                          NA
                        ),
                        exclude = NULL, # inkluderer NA i levels
                        ordered = TRUE
      ),
      Hastegrad = factor(.data$Hastegrad,
                          levels = c(
                            "Elektiv",
                            "Haster",
                            "Akutt",
                            "Under pågående HLR",
                            NA
                          ),
                          exclude = NULL, # inkluderer NA i levels
                          ordered = TRUE),
      PostVenstreVentrikkelFunksjon = factor(
        .data$PostVenstreVentrikkelFunksjon,
        levels = c(
          "Normal",
          "Lett nedsatt: EF 40 - 49% ",
          "Moderat nedsatt: EF 30 - 39%",
          "Betydelig nedsatt: EF 21 - 29%",
          "Alvorlig nedsatt: EF <= 20%",
          "Ukjent",
          NA
          ),
          exclude = NULL, # inkluderer NA i levels
          ordered = TRUE
        ),
      PreVenstreVentrikkelFunksjon = factor(.data$PreVenstreVentrikkelFunksjon,
                                             levels = c(
                                               "Normal",
                                               "Lett nedsatt: EF 40 - 49% ",
                                               "Moderat nedsatt: EF 30 - 39%",
                                               "Betydelig nedsatt: EF 21 - 29%",
                                               "Alvorlig nedsatt: EF <= 20%",
                                               "Ukjent",
                                               NA
                                             ),
                                             exclude = NULL,
                                             ordered = TRUE
      ),
      ProsedyreEkko = factor(.data$ProsedyreEkko,
                               levels = c(
                                 "Nei",
                                 "TEE",
                                 "ICE",
                                 "TTE",
                                 "IVUS",
                                 "Annet",
                                 NA
                               ),
                               exclude = NULL, # inkluderer NA i levels
                               ordered = TRUE
      ),
      UtskrevetTil = factor(.data$UtskrevetTil,
                       levels = c(
                         "Hjem",
                         "Rehabilitering",
                         "Annet sykehus",
                         "Sykehjem",
                         NA
                       ),
                       exclude = NULL, # inkluderer NA i levels
                       ordered = TRUE
      )
    )

  # Utledete variabler:
  mK %<>%
    dplyr::mutate(
      dager_mellom_prosedyre_og_utskr = as.numeric(
        difftime(.data$UtskrDato, .data$ProsedyreDato, units = "days")),
      # Div. tidsvariabler:
      #
      # Kalenderår for ProsedyreDato:
      aar = as.ordered(year(.data$ProsedyreDato)),
      # Måned:
      # (månedsnr er tosifret; 01, 02, ....)
      maaned_nr = as.ordered(sprintf(fmt = "%02d",
                                      month(.data$ProsedyreDato))),
      maaned = as.ordered(paste0(.data$aar, "-", .data$maaned_nr)),
      # Kvartal:
      kvartal = quarter(.data$ProsedyreDato, with_year = TRUE),
      kvartal = as.ordered(gsub("[[:punct:]]", "-Q", .data$kvartal)),
      # Uketall:
      uke = as.ordered(sprintf(fmt = "%02d", isoweek(.data$ProsedyreDato))),

      # Variabel med "yyyy-ukenummer" som tar høyde for uketall spredt over to
      # kalenderår:

      aar_uke = ifelse(
        # hvis uke 01 er i desember...
        test = .data$uke == "01" & .data$maaned_nr == "12",
        # .. så sier vi at uken tilhører det seneste av de to årene som uke 01
        # er spredt over (uke 01 i desember 2019 blir til 2020-01)
        yes = paste0(as.integer(year(.data$ProsedyreDato)) + 1,
                     "-",
                     .data$uke
                     ),
        no = paste0(.data$aar, "-", .data$uke)
      ),
      aar_uke = ifelse(
        # hvis uke 52 eller 53 er i januar...
        test = .data$uke %in% c("52", "53") & .data$maaned_nr == "01",
        # ...sier vi at hele uken tilhører det tidligste av de to årene som uke
        # 52/53 er spredt over (1. januar 2017 som er i uke 52 blir til 2016-52)
        yes = paste0(as.integer(year(.data$ProsedyreDato)) - 1, "-", .data$uke),
        no = .data$aar_uke
      ),
      aar_uke = as.ordered(.data$aar_uke)
    )

  mK

}
