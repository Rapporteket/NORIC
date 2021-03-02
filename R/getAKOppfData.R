#' getAKOppfData provides local reg data from AortaklaffOppfVar
#'
#' @param registryName String providing the registry name
#' @param singleRow Logical defining if only one row is to be returned. A
#' relevant usecase will be when only description is needed. By default set to
#' FALSE
#' @param ... Optional arguments to be passed to the function
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter mutate mutate_all select left_join
#' @importFrom lubridate ymd year month quarter isoweek
#' @importFrom tidyselect ends_with
#'
#' @return Data frame representing the table AortaklaffOppfVar
#' @export
#'

getAKOppfData <- function(registryName, singleRow = FALSE, ...) {

  # declare 'dot'
  . <- ""

  dbType <- "mysql"
  query <- "
SELECT *
FROM AortaklaffOppfVar
  "

  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query metadata for AortaklaffOppfVar pivot"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for AortaklaffOppfVar pivot"
  }

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }

  aKOppf <- rapbase::loadRegData(registryName, query, dbType)

  fO <- rapbase::loadRegData(registryName,
                             query = "SELECT * FROM ForlopsOversikt")


  # Velger relevante variabler fra fO som skal legges til tabellen:
  fO %<>%
    dplyr::select(.,
      # Nøkler:
      .data$AvdRESH,
      .data$ForlopsID,
      # Variablene som legges til:
      .data$Sykehusnavn,
      .data$PasientID,
      .data$PasientKjonn,
      .data$PasientAlder,
      .data$BasisRegStatus,
      .data$ForlopsType1,
      .data$ForlopsType2,
      .data$KobletForlopsID,
      .data$Kommune,
      .data$KommuneNr,
      .data$Fylke,
      .data$Fylkenr,
      .data$FodselsDato,
      .data$Avdod,
      .data$AvdodDato,
      .data$ErOppflg,
      .data$OppflgStatus,
      .data$OppflgSekNr,
      .data$OppflgRegStatus
    )

  # Legger til variabler fra fO til aKOppf:
  aKOppf <- dplyr::left_join(aKOppf, fO, by = c("AvdRESH",
                                                "ForlopsID"
                                         )
  )

  aKOppf

}
