#' Load data for NORIC
#'
#' Functions for loading data needed by NORIC from a database
#'
#' \code{getNameReshId()} returns a mapping of organization name and id in the
#' form of columns named \emph{name} and \emph{id}. Optionally this function
#' can also return a list of named values (ids), \emph{e.g.} for use in shiny
#' selection lists.
#'
#' @param registryName Character string defining the registry name.
#' @param fromDate Character string of format YYYY-MM-DD with start date. Value
#' NULL if no filter on date.
#' @param toDate Character string of format YYYY-MM-DD with end date. Value
#' NULL if no filter on date.
#' @param asNamedList Logical whether to return a list of named values or not.
#' Default is FALSE in which case a data frame containing name and id is
#' returned.
#' @param singleRow Logical if only one row from the table is to be provided.
#' Default value is FALSE.
#' @param reshId Integer dummy/placeholder organization id
#' @param ... Optional arguments to be passed to the function.
#'
#' @return Data frame or (when multiple data sets are returned) a list of data
#' frames containing registry data. In case of \code{getNameReshId()} data may
#' also be returned as a named list of values (see Details).
#' @name getData
#' @aliases getAp getSo getAk getFo getAnP getCt
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
    ForlopsOversikt.ForlopsType2
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
  # 3 variables to match on: AvdRESH, PasientID, ForlopsID

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
    ForlopsOversikt.ForlopsType2
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
