#' Query data for dumps
#'
#' @param registryName String registry name
#' @param tableName String with name of table to query from
#' @param fromDate String with start period, endpoint included
#' @param toDate String with end period, endpont included
#' @param ... Additional parmeters to be passed to the function
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr select left_join
#'
#' @return A data frame with registry data
#' @export
#'

getDataDump <- function(registryName, tableName, fromDate, toDate, ...) {
  # Datadumper som skal filtreres på bakgrunn av ProsedyreDato:
  # AnP, AD, AK, AP, MK & SS
  if (tableName %in% c("AndreProsedyrerVar",
                      "AnnenDiagnostikkVar",
                      "AortaklaffVar",
                      "AngioPCIVar",
                      "MitralklaffVar",
                      "SegmentStent")
  ) {
    query <- paste0("
SELECT
  *
FROM
  ", tableName, "
WHERE
  ProsedyreDato >= '", fromDate, "' AND ProsedyreDato <= '", toDate, "';"
    )
  }


  # Datadumper som skal filtreres på bakgrunn av HovedDato:
  # SO og FO
  if (tableName %in% c("SkjemaOversikt",
                       "ForlopsOversikt")
  ) {
    query <- paste0("
SELECT
  *
FROM
  ", tableName, "
WHERE
  HovedDato >= '", fromDate, "' AND HovedDato <= '", toDate, "';")
  }


  # Datadumper som skal filtreres på bakgrunn av UndersokDato:
  # CT
  if (tableName %in% c("CTAngioVar")
  ) {
    query <- paste0("
SELECT
  *
FROM
  ", tableName, "
WHERE
  UndersokDato >= '", fromDate, "' AND UndersokDato <= '", toDate, "';"
    )
  }


  # Datadumper som skal filtreres på bakgrunn av BasisProsedyreDato:
  # AKOppf
  if (tableName %in% c("AortaklaffOppfVar")
  ) {
    query <- paste0("
SELECT
  *
FROM
  ", tableName, "
WHERE
  BasisProsedyreDato >= '",
                    fromDate,
                    "' AND BasisProsedyreDato <= '",
                    toDate, "';"
    )
  }


  # Datadumper som skal filtreres på bakgrunn av PasInklDato:
  # PS
  if (tableName %in% c("PasienterStudier")
  ) {
    query <- paste0("
SELECT
  *
FROM
  ", tableName, "
WHERE
  PasInklDato >= '", fromDate, "' AND PasInklDato <= '", toDate, "';"
    )
  }




  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]],
                      msg = paste("NORIC data dump:\n", query))
  }


  # Henter tabellen som skal lastes ned av bruker:
  tab <- rapbase::loadRegData(registryName, query)


  # Henter FO, som har felt som skal legges til tabellen (med unntak av når
  # tabellene som skal lastes ned er FO eller SO)
  if (tableName %in% c("AndreProsedyrerVar",
                       "AnnenDiagnostikkVar",
                       "AortaklaffVar",
                       "AortaklaffOppfVar",
                       "AngioPCIVar",
                       "CTAngioVar",
                       "MitralklaffVar",
                       "PasienterStudier",
                       "SegmentStent")) {


    fO <- rapbase::loadRegData(registryName,
                               query = "SELECT * FROM ForlopsOversikt")


    # De forskjellige tabellene har ulike felt fra fO som de har behov for,
    # og ulike nøkler. Derfor får hver tabell sin egen if-setning:


    # AnP ----
    if (tableName %in% c("AndreProsedyrerVar")) {
      fO %<>%
        dplyr::select(
          # Nøkler:
          .data$AvdRESH,
          .data$ForlopsID,
          # Variablene som legges til:
          .data$Sykehusnavn,
          .data$PasientID,
          .data$FodselsDato,
          .data$Kommune,
          .data$KommuneNr,
          .data$Fylke,
          .data$Fylkenr,
          .data$PasientKjonn,
          .data$PasientAlder,
          .data$ForlopsType1,
          .data$ForlopsType2,
          .data$KobletForlopsID,
          .data$HovedDato
        )

      tab <- dplyr::left_join(tab, fO,
                              by = c("ForlopsID", "AvdRESH"),
                              suffix = c("", ".FO")
                              )
    }

    # AD ----
    if (tableName %in% c("AnnenDiagnostikkVar")) {

      fO %<>%
        dplyr::select(
          # Nøkler:
          .data$AvdRESH,
          .data$ForlopsID,
          # Variablene som legges til:
          .data$PasientID,
          .data$PasientAlder,
          .data$Kommune,
          .data$KommuneNr,
          .data$Fylke,
          .data$Fylkenr,
          .data$ForlopsType1,
          .data$ForlopsType2,
          .data$KobletForlopsID,
          .data$HovedDato
        )

      tab <- dplyr::left_join(tab, fO,
                              by = c("ForlopsID", "AvdRESH"),
                              suffix = c("", ".FO")
                              )
    }

    # AK ----
    if (tableName %in% c("AortaklaffVar")) {

      fO %<>%
        dplyr::select(
          # Nøkler:
          .data$AvdRESH,
          .data$ForlopsID,
          # Variablene som legges til:
          .data$Sykehusnavn,
          .data$PasientID,
          .data$FodselsDato,
          .data$Kommune,
          .data$KommuneNr,
          .data$Fylke,
          .data$Fylkenr,
          .data$PasientKjonn,
          .data$PasientAlder,
          .data$ForlopsType1,
          .data$ForlopsType2,
          .data$KobletForlopsID,
          .data$HovedDato,
          .data$Avdod
        )

      tab <- dplyr::left_join(tab, fO,
                              by = c("ForlopsID", "AvdRESH"),
                              suffix = c("", ".FO")
                              )
    }


    # AKOppf ----
    if (tableName %in% c("AortaklaffOppfVar")) {

      fO %<>%
        dplyr::select(
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
          .data$HovedDato,
          .data$Kommune,
          .data$KommuneNr,
          .data$Fylke,
          .data$Fylkenr,
          .data$FodselsDato,
          .data$ErOppflg,
          .data$OppflgStatus,
          .data$OppflgSekNr,
          .data$OppflgRegStatus
        )

      tab <- dplyr::left_join(tab, fO,
                              by = c("ForlopsID", "AvdRESH"),
                              suffix = c("", ".FO")
      )
    }


    # AP ----
    if (tableName %in% c("AngioPCIVar")) {

      fO %<>%
        dplyr::select(
          # Nøkler:
          .data$AvdRESH,
          .data$Sykehusnavn,
          .data$PasientID,
          .data$ForlopsID,
          # Variablene som legges til:
          .data$Kommune,
          .data$KommuneNr,
          .data$Fylke,
          .data$Fylkenr,
          .data$PasientAlder,
          .data$ForlopsType1,
          .data$ForlopsType2,
          .data$KobletForlopsID,
          .data$HovedDato
        )

      # Legger til variabler fra fO til AP:
      tab <- dplyr::left_join(tab, fO, by = c("AvdRESH",
                                              "Sykehusnavn",
                                              "PasientID",
                                              "ForlopsID")
      )
    }


    # CT ----
    if (tableName %in% c("CTAngioVar")) {

      fO %<>%
        dplyr::select(
          # Nøkler:
          .data$AvdRESH,
          .data$ForlopsID,
          .data$PasientID,
          # Variablene som legges til:
          .data$Sykehusnavn,
          .data$Kommune,
          .data$KommuneNr,
          .data$Fylke,
          .data$Fylkenr,
          .data$PasientKjonn,
          .data$PasientAlder,
          .data$ForlopsType1,
          .data$ForlopsType2,
          .data$KobletForlopsID,
          .data$HovedDato
        )

      tab <- dplyr::left_join(tab, fO, by = c("ForlopsID",
                                              "PasientID",
                                              "AvdRESH"),
                              suffix = c("", ".FO")
                              )
    }


    # MK ----
    if (tableName %in% c("MitralklaffVar")) {

      fO %<>%
        dplyr::select(
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
          .data$HovedDato,
          .data$FodselsDato
        )

      tab <- dplyr::left_join(tab, fO, by = c("ForlopsID", "AvdRESH"),
                              suffix = c("", ".FO")
                              )
    }


    # PS ----
    if (tableName %in% c("PasienterStudier")) {

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

      tab <- dplyr::left_join(tab, fO, by = c("PasientID", "AvdRESH"),
                              suffix = c("", ".FO")
                              )
    }



    # SS ----
    if (tableName %in% c("SegmentStent")) {

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
          .data$KobletForlopsID,
          .data$HovedDato
          )

      tab <- dplyr::left_join(tab, fO, by = c("ForlopsID",
                                              "AvdRESH",
                                              "Sykehusnavn"),
                              suffix = c("", ".FO")
                              )
    }
  }

}
