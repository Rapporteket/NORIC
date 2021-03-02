#' getLocalSOData provides local reg data from SkjemaOversikt
#'
#' @param registryName String providing the registry name
#' @param singleRow Logical defining if only one row is to be returned. A
#' relevant usecase will be when only description is needed. By default set to
#' FALSE
#' @param ... Optional arguments to be passed to the function
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter mutate mutate_all select
#' @importFrom lubridate ymd year month quarter isoweek
#'
#' @return Data frame representing the table SkjemaOversikt
#' @export
#'

getLocalSOData <- function(registryName, singleRow = FALSE, ...) {

  dbType <- "mysql"
  query <- "SELECT * FROM SkjemaOversikt"

  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query metadata for SkjemaOversikt pivot"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for SkjemaOversikt pivot"
  }

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }

  sO <- rapbase::loadRegData(registryName, query, dbType)



  # Gjor datoer om til dato-objekt:
  sO %<>%
    dplyr::mutate(
      OpprettetDato = lubridate::ymd_hms(.data$OpprettetDato),
      SistLagretDato = lubridate::ymd_hms(.data$SistLagretDato),
      HovedDato = lubridate::ymd(.data$HovedDato)
    )


  # Endre Sykehusnavn til kortere versjoner:
  sO %<>%
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
  sO %<>%
    dplyr::filter(
      (
        # HUS
        (.data$AvdRESH == 102966) & (as.Date(.data$HovedDato) >= "2013-01-01")
      ) | (
        # UNN
        (.data$AvdRESH == 101619) & (as.Date(.data$HovedDato) >= "2013-05-01")
      ) | (
        # Ullevaal
        (.data$AvdRESH == 109880) & (as.Date(.data$HovedDato) >= "2014-01-01")
      ) | (
        # St Olav
        (.data$AvdRESH == 104284) & (as.Date(.data$HovedDato) >= "2014-01-01")
      ) | (
        # SSA
        (.data$AvdRESH == 114150) & (as.Date(.data$HovedDato) >= "2014-01-01")
      ) | (
        # SUS
        (.data$AvdRESH == 105502) & (as.Date(.data$HovedDato) >= "2014-01-01")
      ) | (
        # Risksen
        (.data$AvdRESH == 700422) & (as.Date(.data$HovedDato) >= "2015-01-01")
      ) | (
        # LHL Gardermoen
        (.data$AvdRESH == 106944) & (as.Date(.data$HovedDato) >= "2015-01-01")
      ) | (
        # Ahus
        (.data$AvdRESH == 108141) & (as.Date(.data$HovedDato) >= "2016-01-01")
      ) | (
        # Bodø
        (.data$AvdRESH == 4210141) & (as.Date(.data$HovedDato) >= "2020-02-10")
      )
    )


  # Utledete variabler:
  sO %<>%
    dplyr::mutate(
      # Ferdigstilt, 1= ja, -1 & 0 = nei
      ferdigstilt = as.ordered(ifelse(.data$SkjemaStatus == 1,
                                      yes = "Ferdigstilt",
                                      no = "Ikke ferdigstilt")
      ),
      # Div. tidsvariabler:
      #
      # Kalenderår for HovedDato:
      aar = as.ordered(lubridate::year(.data$HovedDato)),
      # Måned:
      # (månedsnr er tosifret; 01, 02, ....)
      maaned_nr = as.ordered(sprintf(fmt = "%02d",
                                     lubridate::month(.data$HovedDato))),
      maaned = as.ordered(paste0(.data$aar, "-", .data$maaned_nr)),
      # Kvartal:
      kvartal = lubridate::quarter(.data$HovedDato, with_year = TRUE),
      kvartal = as.ordered(gsub("[[:punct:]]", "-Q", .data$kvartal)),
      # Uketall:
      uke = as.ordered(sprintf(fmt = "%02d",
                               lubridate::isoweek(.data$HovedDato))),
      # Variabel med "yyyy-ukenummer" som tar høyde for uketall spredt over to
      # kalenderår:
      aar_uke = ifelse(
        # hvis uke 01 er i desember...
        test = .data$uke == "01" & .data$maaned_nr == "12",
        # .. så sier vi at uken tilhører det seneste av de to årene som uke 01
        # er spredt over (uke 01 i desember 2019 blir til 2020-01)
        yes = paste0(as.integer(lubridate::year(.data$HovedDato)) + 1, "-",
                     .data$uke),
        no = paste0(.data$aar, "-", .data$uke)
      ),
      aar_uke = ifelse(
        # hvis uke 52 eller 53 er i januar...
        test = .data$uke %in% c("52", "53") & .data$maaned_nr == "01",
        # ...sier vi at hele uken tilhører det tidligste av de to årene som uke
        # 52/53 er spredt over (1. januar 2017 som er i uke 52 blir til 2016-52)
        yes = paste0(as.integer(lubridate::year(.data$HovedDato)) - 1, "-",
                        .data$uke),
        no = .data$aar_uke
      ),
      aar_uke = as.ordered(.data$aar_uke)
    )

  sO

}
