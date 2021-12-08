#' Load data for NORIC
#'
#' Functions for loading data needed by NORIC from a database
#'
#' The functions \code{getAp}, \code{getSs}, \code{getAk} etc. load the
#' AP-, SS- and AK- tables etc. respectively. For most tables, the query adds
#' selected variables from  \emph{ForlopsOversikt}.
#'
#' @param registryName Character string defining the registry name.
#' @param fromDate Character string of format YYYY-MM-DD with start date. Value
#' NULL if no filter on date.
#' @param toDate Character string of format YYYY-MM-DD with end date. Value
#' NULL if no filter on date.
#' @param singleRow Logical if only one row from the table is to be provided.
#' Default value is FALSE.
#' @param ... Optional arguments to be passed to the function.
#'
#' @return Data frame or (when multiple data sets are returned) a list of data
#' frames containing registry data. In case of \code{getNameReshId()} data may
#' also be returned as a named list of values (see Details).
#' @name getData
#' @aliases getAp
#' getSo
#' getAk
#' getFo
#' getAnP
#' getCt
#' getAkOppf
#' getAnD
#' getSs
#' getMk
#' getPs
#' getApLight
#' NULL
#'
#'
getAp <- function(registryName, fromDate, toDate, singleRow, ...) {


  # SQL possible for defined time-interval:
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- noric::getLatestEntry(registryName)
  }

  # Ask for all variables from ANGIO PCI in time interval
  # Add selected variables from ForlopsOversikt
  # 3 variables to match on: AvdRESH, PasientID, ForlopsID

  query <- paste0("
SELECT
    AngioPCIVar.*,
    ForlopsOversikt.Kommune,
    ForlopsOversikt.KommuneNr,
    ForlopsOversikt.Fylke,
    ForlopsOversikt.Fylkenr,
    ForlopsOversikt.PasientAlder,
    ForlopsOversikt.KobletForlopsID
FROM
    AngioPCIVar
WHERE
    ProsedyreDato >= '", fromDate, "' AND
    ProsedyreDato <= '", toDate, "'
LEFT JOIN ForlopsOversikt ON
    AngioPCIVar.AvdRESH = ForlopsOversikt.AvdRESH AND
    AngioPCIVar.PasientID = ForlopsOversikt.PasientID AND
    AngioPCIVar.ForlopsID = ForlopsOversikt.ForlopsID
 ")

  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for AngioPCI"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for AngioPCI"
  }

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }

  aP <- rapbase::loadRegData(registryName, query, dbType)



  list(aP = aP)
}


getSo <- function(registryName, fromDate, toDate, singleRow, ...) {


  # SQL possible for defined time-interval:
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- noric::getLatestEntry(registryName)
  }


  # Ask for all variables from SKJEMAOVERSIKT, in time interval
  query <- paste0("
SELECT
    *
FROM
    SkjemaOversikt
WHERE
    HovedDato >= '", fromDate, "' AND
    HovedDato <= '", toDate, "'
 ")


  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for SkjemaOversikt"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for SkjemaOversikt"
  }

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }

  sO <- rapbase::loadRegData(registryName, query, dbType)


  list(sO = sO)

}


getAk <- function(registryName, fromDate, toDate, singleRow, ...){

  # SQL possible for defined time-interval:
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- noric::getLatestEntry(registryName)
  }

  # Ask for all variables from AORTAKLAFFVAR in time interval
  # Add selected variables from ForlopsOversikt
  # 2 variables to match on: AvdRESH, ForlopsID

  query <- paste0("
SELECT
    AortaklaffVar.*,
    ForlopsOversikt.Sykehusnavn,
    ForlopsOversikt.PasientID,
    ForlopsOversikt.FodselsDato,
    ForlopsOversikt.Kommune,
    ForlopsOversikt.KommuneNr,
    ForlopsOversikt.Fylke,
    ForlopsOversikt.Fylkenr,
    ForlopsOversikt.PasientKjonn,
    ForlopsOversikt.PasientAlder,
    ForlopsOversikt.ForlopsType1,
    ForlopsOversikt.ForlopsType2,
    ForlopsOversikt.KobletForlopsID,
    ForlopsOversikt.Avdod
FROM
    AortaklaffVar
WHERE
    ProsedyreDato >= '", fromDate, "' AND
    ProsedyreDato <= '", toDate, "'
LEFT JOIN ForlopsOversikt ON
    AortaklaffVar.AvdRESH = ForlopsOversikt.AvdRESH AND
    AortaklaffVar.ForlopsID = ForlopsOversikt.ForlopsID
 ")

  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for AortaklaffVar"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for AortaklaffVar"
  }

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }

  aK <- rapbase::loadRegData(registryName, query, dbType)



  list(aK = aK)
}


getFo <- function(registryName, fromDate, toDate, singleRow, ...) {


  # SQL possible for defined time-interval:
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- noric::getLatestEntry(registryName)
  }


  # Ask for all variables from ForlopsOversikt, in time interval
  query <- paste0("
SELECT
    *
FROM
    ForlopsOversikt
WHERE
    HovedDato >= '", fromDate, "' AND
    HovedDato <= '", toDate, "'
 ")


  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for ForlopsOversikt"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for ForlopsOversikt"
  }

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }

  fO <- rapbase::loadRegData(registryName, query, dbType)


  list(fO = fO)

}


getAnP <- function(registryName, fromDate, toDate, singleRow, ...){

  # SQL possible for defined time-interval:
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- noric::getLatestEntry(registryName)
  }

  # Ask for all variables from AndreProsedyrerVar in time interval
  # Add selected variables from ForlopsOversikt
  # 2 variables to match on: AvdRESH, ForlopsID

  query <- paste0("
SELECT
    AndreProsedyrerVar.*,
    ForlopsOversikt.Sykehusnavn,
    ForlopsOversikt.PasientID,
    ForlopsOversikt.FodselsDato,
    ForlopsOversikt.Kommune,
    ForlopsOversikt.KommuneNr,
    ForlopsOversikt.Fylke,
    ForlopsOversikt.Fylkenr,
    ForlopsOversikt.PasientKjonn,
    ForlopsOversikt.PasientAlder,
    ForlopsOversikt.ForlopsType1,
    ForlopsOversikt.ForlopsType2,
    ForlopsOversikt.KobletForlopsID
FROM
    AndreProsedyrerVar
WHERE
    ProsedyreDato >= '", fromDate, "' AND
    ProsedyreDato <= '", toDate, "'
LEFT JOIN ForlopsOversikt ON
    AndreProsedyrerVar.AvdRESH = ForlopsOversikt.AvdRESH AND
    AndreProsedyrerVar.ForlopsID = ForlopsOversikt.ForlopsID
 ")

  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for AndreProsedyrerVar"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for AndreProsedyrerVar"
  }

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }

  anP <- rapbase::loadRegData(registryName, query, dbType)



  list(anP = anP)
}


getCt <- function(registryName, fromDate, toDate, singleRow, ...){

  # SQL possible for defined time-interval:
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- noric::getLatestEntry(registryName)
  }

  # Ask for all variables from CT in time interval
  # Add selected variables from ForlopsOversikt
  # 3 variables to match on: AvdRESH, ForlopsID

  query <- paste0("
SELECT
    CTAngioVar.*,
    ForlopsOversikt.Sykehusnavn,
    ForlopsOversikt.Kommune,
    ForlopsOversikt.KommuneNr,
    ForlopsOversikt.Fylke,
    ForlopsOversikt.Fylkenr,
    ForlopsOversikt.PasientKjonn,
    ForlopsOversikt.PasientAlder,
    ForlopsOversikt.ForlopsType1,
    ForlopsOversikt.ForlopsType2,
    ForlopsOversikt.KobletForlopsID
FROM
    CTAngioVar
WHERE
    UndersokDato >= '", fromDate, "' AND
    UndersokDato <= '", toDate, "'
LEFT JOIN ForlopsOversikt ON
    CTAngioVar.AvdRESH = ForlopsOversikt.AvdRESH AND
    CTAngioVar.ForlopsID = ForlopsOversikt.ForlopsID
 ")

  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for CTAngioVar"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for CTAngioVar"
  }

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }

  cT <- rapbase::loadRegData(registryName, query, dbType)



  list(cT = cT)
}


getAkOppf <- function(registryName, fromDate, toDate, singleRow, ...){

  # SQL possible for defined time-interval:
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- noric::getLatestEntry(registryName)
  }

  # Ask for all variables from AortaklaffOppfVar in time interval
  # Add selected variables from ForlopsOversikt
  # 2 variables to match on: AvdRESH, ForlopsID

  query <- paste0("
SELECT
    AortaklaffOppfVar.*,
    ForlopsOversikt.Sykehusnavn,
    ForlopsOversikt.PasientID,
    ForlopsOversikt.FodselsDato,
    ForlopsOversikt.BasisRegStatus,
    ForlopsOversikt.Kommune,
    ForlopsOversikt.KommuneNr,
    ForlopsOversikt.Fylke,
    ForlopsOversikt.Fylkenr,
    ForlopsOversikt.PasientKjonn,
    ForlopsOversikt.PasientAlder,
    ForlopsOversikt.ForlopsType1,
    ForlopsOversikt.ForlopsType2,
    ForlopsOversikt.KobletForlopsID,
    ForlopsOversikt.Avdod,
    ForlopsOversikt.AvdodDato,
    ForlopsOversikt.ErOppflg ,
    ForlopsOversikt.OppflgStatus,
    ForlopsOversikt.OppflgSekNr,
    ForlopsOversikt.OppflgRegStatus
FROM
    AortaklaffOppfVar
WHERE
    BasisProsedyreDato >= '", fromDate, "' AND
    BasisProsedyreDato <= '", toDate, "'
LEFT JOIN ForlopsOversikt ON
    AortaklaffOppfVar.AvdRESH = ForlopsOversikt.AvdRESH AND
    AortaklaffOppfVar.ForlopsID = ForlopsOversikt.ForlopsID
 ")

  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for AortaklaffOppfVar"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for AortaklaffOppfVar"
  }

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }

  aKoppf <- rapbase::loadRegData(registryName, query, dbType)



  list(aKoppf = aKoppf)
}


getAnD <- function(registryName, fromDate, toDate, singleRow, ...){

  # SQL possible for defined time-interval:
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- noric::getLatestEntry(registryName)
  }

  # Ask for all variables from AnnenDiagnostikkVar in time interval
  # Add selected variables from ForlopsOversikt
  # 2 variables to match on: AvdRESH, ForlopsID

  query <- paste0("
SELECT
    AnnenDiagnostikkVar.*,
    ForlopsOversikt.PasientID,
    ForlopsOversikt.Kommune,
    ForlopsOversikt.KommuneNr,
    ForlopsOversikt.Fylke,
    ForlopsOversikt.Fylkenr,
    ForlopsOversikt.PasientAlder,
    ForlopsOversikt.ForlopsType1,
    ForlopsOversikt.ForlopsType2,
    ForlopsOversikt.KobletForlopsID
FROM
    AnnenDiagnostikkVar
WHERE
    ProsedyreDato >= '", fromDate, "' AND
    ProsedyreDato <= '", toDate, "'
LEFT JOIN ForlopsOversikt ON
    AnnenDiagnostikkVar.AvdRESH = ForlopsOversikt.AvdRESH AND
    AnnenDiagnostikkVar.ForlopsID = ForlopsOversikt.ForlopsID
 ")

  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for AnnenDiagnostikkVar"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for AnnenDiagnostikkVar"
  }

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }

  AnD <- rapbase::loadRegData(registryName, query, dbType)



  list(AnD = AnD)
}


getSS <- function(registryName, fromDate, toDate, singleRow, ...) {


  # SQL possible for defined time-interval:
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- noric::getLatestEntry(registryName)
  }

  # Ask for all variables from Segment STent in time interval
  # Add selected variables from ForlopsOversikt
  # 2 variables to match on: AvdRESH, ForlopsID

  query <- paste0("
SELECT
    SegmentStent.*,
    ForlopsOversikt.PasientID,
    ForlopsOversikt.Kommune,
    ForlopsOversikt.KommuneNr,
    ForlopsOversikt.Fylke,
    ForlopsOversikt.Fylkenr,
    ForlopsOversikt.PasientAlder,
    ForlopsOversikt.ForlopsType1,
    ForlopsOversikt.ForlopsType2,
    ForlopsOversikt.KobletForlopsID
FROM
    SegmentStent
WHERE
    ProsedyreDato >= '", fromDate, "' AND
    ProsedyreDato <= '", toDate, "'
LEFT JOIN ForlopsOversikt ON
    SegmentStent.AvdRESH = ForlopsOversikt.AvdRESH AND
    SegmentStent.ForlopsID = ForlopsOversikt.ForlopsID
 ")

  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for SegmentStent"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for SegmentStent"
  }

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }

  sS <- rapbase::loadRegData(registryName, query, dbType)



  list(sS = sS)
}


getMk <- function(registryName, fromDate, toDate, singleRow, ...){

  # SQL possible for defined time-interval:
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- noric::getLatestEntry(registryName)
  }

  # Ask for all variables from mitralklaff in time interval
  # Add selected variables from ForlopsOversikt
  # 2 variables to match on: AvdRESH, ForlopsID

  query <- paste0("
SELECT
    MitralklaffVar.*,
    ForlopsOversikt.Sykehusnavn,
    ForlopsOversikt.PasientID,
    ForlopsOversikt.FodselsDato,
    ForlopsOversikt.Kommune,
    ForlopsOversikt.KommuneNr,
    ForlopsOversikt.Fylke,
    ForlopsOversikt.Fylkenr,
    ForlopsOversikt.PasientKjonn,
    ForlopsOversikt.PasientAlder,
    ForlopsOversikt.ForlopsType1,
    ForlopsOversikt.ForlopsType2,
    ForlopsOversikt.KobletForlopsID,
    ForlopsOversikt.BasisRegStatus,

FROM
    MitralklaffVar
WHERE
    ProsedyreDato >= '", fromDate, "' AND
    ProsedyreDato <= '", toDate, "'
LEFT JOIN ForlopsOversikt ON
    MitralklaffVar.AvdRESH = ForlopsOversikt.AvdRESH AND
    MitralklaffVar.ForlopsID = ForlopsOversikt.ForlopsID
 ")

  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for MitralklaffVar"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for MitralklaffVar"
  }

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }

  mK <- rapbase::loadRegData(registryName, query, dbType)



  list(mK = mK)
}


getPs <- function(registryName, fromDate, toDate, singleRow, ...){

  # SQL possible for defined time-interval:
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- noric::getLatestEntry(registryName)
  }

  # Ask for all variables from PAsientstudier in time interval
  # Add selected variables from ForlopsOversikt
  # 2 variables to match on: AvdRESH, ForlopsID

  query <- paste0("
SELECT
    PasienterStudier.*,
    ForlopsOversikt.Sykehusnavn,
    ForlopsOversikt.FodselsDato,
    ForlopsOversikt.Kommune,
    ForlopsOversikt.KommuneNr,
    ForlopsOversikt.Fylke,
    ForlopsOversikt.Fylkenr,
    ForlopsOversikt.PasientKjonn,
    ForlopsOversikt.PasientAlder

FROM
    PasienterStudier
WHERE
    ProsedyreDato >= '", fromDate, "' AND
    ProsedyreDato <= '", toDate, "'
LEFT JOIN ForlopsOversikt ON
    PasienterStudier.AvdRESH = ForlopsOversikt.AvdRESH AND
    PasienterStudier.PasientID = ForlopsOversikt.PasientID
 ")

  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for PasienterStudier"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for PasienterStudier"
  }

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }

  pS <- rapbase::loadRegData(registryName, query, dbType)



  list(pS = pS)
}


getApLight <- function(registryName, fromDate, toDate, singleRow, ...) {


  # SQL possible for defined time-interval
  # If no other date defined, load tables from 3 last years until today:
  if (is.null(fromDate)) {
    latest_entry <- noric::getLatestEntry(registryName = registryName)
    fromDate <- as.Date(
      paste0(as.numeric(lubridate::year(latest_entry)) - 3, "-01-01"),
      format = "%Y-%m-%d")
  }
  if (is.null(toDate)) {
    toDate <- noric::getLatestEntry(registryName)
  }


  # QUERY ANGIO PCI + FO
  # Ask for all variables from ANGIO PCI in time interval
  # Add selected variables from ForlopsOversikt
  # 3 variables to match on: AvdRESH, PasientID, ForlopsID

  query <- paste0("
SELECT
    AngioPCIVar.*,
    ForlopsOversikt.Kommune,
    ForlopsOversikt.KommuneNr,
    ForlopsOversikt.Fylke,
    ForlopsOversikt.Fylkenr,
    ForlopsOversikt.PasientAlder,
    ForlopsOversikt.KobletForlopsID
FROM
    AngioPCIVar
WHERE
    ProsedyreDato >= '", fromDate, "' AND
    ProsedyreDato <= '", toDate, "'
LEFT JOIN ForlopsOversikt ON
    AngioPCIVar.AvdRESH = ForlopsOversikt.AvdRESH AND
    AngioPCIVar.PasientID = ForlopsOversikt.PasientID AND
    AngioPCIVar.ForlopsID = ForlopsOversikt.ForlopsID
 ")



  # Only ask for variables needed in functions:utlede_segment_stent_variabler.R
  querySs <- paste0("
SELECT
    ForlopsID, AvdRESH, StentType, Segment, Graft, ProsedyreType
FROM
    SegmentStent
WHERE
    ProsedyreDato >= '", fromDate, "' AND
    ProsedyreDato <= '", toDate, "'
")


#   # Only ask for variables needed in functions: utlede_annen_diag_variabler.R
#   queryAd <- paste0("
# SELECT
#     ForlopsID, AvdRESH, segment, graft, metode
# FROM
#     AnnenDiagnostikkVar
# WHERE
#     ProsedyreDato >= '", fromDate, "' AND
#     ProsedyreDato <= '", toDate, "'
# ")


  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    # queryAd <- paste0(queryAd, "\nLIMIT\n  1;")
    querySs <- paste0(querySs, "\nLIMIT\n  1;")
    msg <- "Query single row data for AngioPCI light"
  } else {
    query <- paste0(query, ";")
    # queryAd <- paste0(queryAd, ";")
    querySs <- paste0(querySs, ";")
    msg <- "Query data for AngioPCI light"
  }

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }

  aP <- rapbase::loadRegData(registryName, query, dbType)
  # aD <- rapbase::loadRegData(registryName, queryAd, dbType)
  sS <- rapbase::loadRegData(registryName, querySs, dbType)



  list(aP = aP,
       # aD = aD,
       sS = sS)
}

