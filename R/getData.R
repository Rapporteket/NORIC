#' Load data for NORIC
#'
#' Functions for loading data needed by NORIC from a database
#'
#' The functions \code{getAp}, \code{getSs}, \code{getAk} etc. load the
#' AP-, SS- and AK- tables etc. respectively. For most tables, the query adds
#' selected variables from  \emph{forlopsoversikt}.
#'
#' @param registryName Character string defining the registry name.
#' @param fromDate Character string of format YYYY-MM-DD with start date. Value
#' NULL if no filter on date.
#' @param toDate Character string of format YYYY-MM-DD with end date. Value
#' NULL if no filter on date.
#' @param singleRow Logical if only one row from the table is to be provided.
#' Default value is FALSE.
#' @param singleHospital if only data from one hospital, when national database. 
#' Default value is NULL, contains reshID of selected hospital else. 
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
#' getSh
#' getMk
#' getPs
#' getApLight
#' getTaviProm
NULL

#' @rdname getData
#' @export
getAp <- function(registryName, fromDate, toDate, singleRow, 
                  singleHospital = NULL, ...) {
  
  
  # SQL possible for defined time-interval:
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- noric::getLatestEntry(registryName)
  }
  
  # Ask for all variables from ANGIO PCI in time interval
  # Add selected variables from forlopsoversikt
  # 3 variables to match on: AvdRESH, PasientID, ForlopsID
  
    query <- paste0("
SELECT
    a.*,
    f.Kommune,
    f.KommuneNr,
    f.Fylke,
    f.Fylkenr,
    f.PasientAlder,
    f.KobletForlopsID,
    f.ForlopsType2
FROM
      angiopcinum a
LEFT JOIN forlopsoversikt f ON
    a.AvdRESH = f.AvdRESH AND
    a.PasientID = f.PasientID AND
    a.ForlopsID = f.ForlopsID
WHERE
      a.ProsedyreDato >= '", fromDate, "' AND
      a.ProsedyreDato <= '", toDate, "'"
    )

  if(!is.null(singleHospital)) {
    query <- paste0(query,
                    "AND a.AvdRESH = ",
                    singleHospital)
  }

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

  aPnum <- rapbase::loadRegData(registryName, query)
  aP <- noric::erstatt_koder_m_etiketter(aPnum, 
                                         mapping = noric::angp_map_num_tekst)
  
  list(aP = aP)
}


#' @rdname getData
#' @export
getSo <- function(registryName, fromDate, toDate, singleRow, ...) {
  
  
  # SQL possible for defined time-interval:
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- noric::getLatestEntry(registryName)
  }
  
  
  # Ask for all variables from skjemaoversikt, in time interval
  query <- paste0("
SELECT
    *
FROM
    skjemaoversikt
WHERE
    HovedDato >= '", fromDate, "' AND
    HovedDato <= '", toDate, "'
 ")
  
  
  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for skjemaoversikt"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for skjemaoversikt"
  }
  
  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  sO <- rapbase::loadRegData(registryName, query)
  
  
  list(sO = sO)
  
}


#' @rdname getData
#' @export
getAk <- function(registryName, fromDate, toDate, singleRow, 
                  singleHospital = NULL,
                  ...){
  
  # SQL possible for defined time-interval:
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- noric::getLatestEntry(registryName)
  }
  
  # Ask for all variables from aortaklaffvar in time interval
  # Add selected variables from forlopsoversikt
  # 2 variables to match on: AvdRESH, ForlopsID
  
  query <- paste0("
SELECT
    aortaklaffvarnum.*,
    forlopsoversikt.Sykehusnavn,
    forlopsoversikt.FodselsDato,
    forlopsoversikt.Kommune,
    forlopsoversikt.KommuneNr,
    forlopsoversikt.Fylke,
    forlopsoversikt.Fylkenr,
    forlopsoversikt.PasientKjonn,
    forlopsoversikt.PasientAlder,
    forlopsoversikt.ForlopsType1,
    forlopsoversikt.ForlopsType2,
    forlopsoversikt.KobletForlopsID,
    forlopsoversikt.Avdod
FROM
    aortaklaffvarnum
LEFT JOIN forlopsoversikt ON
    aortaklaffvarnum.AvdRESH = forlopsoversikt.AvdRESH AND
    aortaklaffvarnum.ForlopsID = forlopsoversikt.ForlopsID
WHERE
    ProsedyreDato >= '", fromDate, "' AND
    ProsedyreDato <= '", toDate, "'"
  )
  
  
  if(!is.null(singleHospital)) {
    query <- paste0(query, 
                    "AND aortaklaffvarnum.AvdRESH = ", 
                    singleHospital)
  }
  
  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for aortaklaffvarnum"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for aortaklaffvarnum"
  }
  
  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  aKnum <- rapbase::loadRegData(registryName, query)
  aK <- noric::erstatt_koder_m_etiketter(aKnum,
                                         mapping = noric::aort_map_num_tekst)
  
  
  
  list(aK = aK)
}


#' @rdname getData
#' @export
getFo <- function(registryName, fromDate, toDate, singleRow, ...) {
  
  
  # SQL possible for defined time-interval:
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- noric::getLatestEntry(registryName)
  }
  
  
  # Ask for all variables from forlopsoversikt, in time interval
  query <- paste0("
SELECT
    *
FROM
    forlopsoversikt
WHERE
    HovedDato >= '", fromDate, "' AND
    HovedDato <= '", toDate, "'
 ")
  
  
  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for forlopsoversikt"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for forlopsoversikt"
  }
  
  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  fO <- rapbase::loadRegData(registryName, query)
  
  
  list(fO = fO)
  
}


#' @rdname getData
#' @export
getAnP <- function(registryName, fromDate, toDate, singleRow,
                   singleHospital = NULL, ...) {                  
  
  # SQL possible for defined time-interval:
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- noric::getLatestEntry(registryName)
  }
  
  # Ask for all variables from andreprosedyrervar in time interval
  # Add selected variables from forlopsoversikt
  # 2 variables to match on: AvdRESH, ForlopsID
  
  query <- paste0("
SELECT
    andreprosedyrervarnum.*,
    forlopsoversikt.Sykehusnavn,
    forlopsoversikt.PasientID,
    forlopsoversikt.FodselsDato,
    forlopsoversikt.Kommune,
    forlopsoversikt.KommuneNr,
    forlopsoversikt.Fylke,
    forlopsoversikt.Fylkenr,
    forlopsoversikt.PasientKjonn,
    forlopsoversikt.PasientAlder,
    forlopsoversikt.ForlopsType1,
    forlopsoversikt.ForlopsType2,
    forlopsoversikt.KobletForlopsID
FROM
    andreprosedyrervarnum
LEFT JOIN forlopsoversikt ON
    andreprosedyrervarnum.AvdRESH = forlopsoversikt.AvdRESH AND
    andreprosedyrervarnum.ForlopsID = forlopsoversikt.ForlopsID
WHERE
    andreprosedyrervarnum.ProsedyreDato >= '", fromDate, "' AND
    andreprosedyrervarnum.ProsedyreDato <= '", toDate, "'"
  )
  
  if(!is.null(singleHospital)) {
    query <- paste0(query, 
                    "AND andreprosedyrervarnum.AvdRESH = ", 
                    singleHospital)
  }
  
  
  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for andreprosedyrervarnum"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for andreprosedyrervarnum"
  }
  
  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  anPnum <- rapbase::loadRegData(registryName, query)
  anP <- noric::erstatt_koder_m_etiketter(anPnum,
                                          mapping = noric::APVN_map_num_tekst)
  
  
  list(anP = anP)
}


#' @rdname getData
#' @export
getCt <- function(registryName, fromDate, toDate, singleRow, ...){
  
  # SQL possible for defined time-interval:
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- noric::getLatestEntry(registryName)
  }
  
  # Ask for all variables from CT in time interval
  # Add selected variables from forlopsoversikt
  # 3 variables to match on: AvdRESH, ForlopsID
  
  query <- paste0("
SELECT
    ctangiovarnum.*,
    forlopsoversikt.Sykehusnavn,
    forlopsoversikt.Kommune,
    forlopsoversikt.KommuneNr,
    forlopsoversikt.Fylke,
    forlopsoversikt.Fylkenr,
    forlopsoversikt.PasientKjonn,
    forlopsoversikt.PasientAlder,
    forlopsoversikt.ForlopsType1,
    forlopsoversikt.ForlopsType2,
    forlopsoversikt.KobletForlopsID
FROM
    ctangiovarnum
LEFT JOIN forlopsoversikt ON
    ctangiovarnum.AvdRESH = forlopsoversikt.AvdRESH AND
    ctangiovarnum.ForlopsID = forlopsoversikt.ForlopsID
WHERE
    ctangiovarnum.UndersokDato >= '", fromDate, "' AND
    ctangiovarnum.UndersokDato <= '", toDate, "'"
  )
  
  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for ctangiovarnum"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for ctangiovarnum"
  }
  
  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  cTnum <- rapbase::loadRegData(registryName, query)
  cT <-  noric::erstatt_koder_m_etiketter(cTnum,
                                          mapping = noric::CTANG_map_num_tekst)
  
  list(cT = cT)
}


#' @rdname getData
#' @export
getAkOppf <- function(registryName, fromDate, toDate, singleRow, ...){
  
  # SQL possible for defined time-interval:
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- noric::getLatestEntry(registryName)
  }
  
  # Ask for all variables from aortaklaffoppfvarnum in time interval
  # Add selected variables from forlopsoversikt
  # 2 variables to match on: AvdRESH, ForlopsID
  
  query <- paste0("
SELECT
    aortaklaffoppfvarnum.*,
    forlopsoversikt.Sykehusnavn,
    forlopsoversikt.PasientID,
    forlopsoversikt.FodselsDato,
    forlopsoversikt.BasisRegStatus,
    forlopsoversikt.Kommune,
    forlopsoversikt.KommuneNr,
    forlopsoversikt.Fylke,
    forlopsoversikt.Fylkenr,
    forlopsoversikt.PasientKjonn,
    forlopsoversikt.PasientAlder,
    forlopsoversikt.ForlopsType1,
    forlopsoversikt.ForlopsType2,
    forlopsoversikt.KobletForlopsID,
    forlopsoversikt.Avdod,
    forlopsoversikt.AvdodDato,
    forlopsoversikt.ErOppflg ,
    forlopsoversikt.OppflgStatus,
    forlopsoversikt.OppflgSekNr,
    forlopsoversikt.OppflgRegStatus
FROM
    aortaklaffoppfvarnum
LEFT JOIN forlopsoversikt ON
    aortaklaffoppfvarnum.AvdRESH = forlopsoversikt.AvdRESH AND
    aortaklaffoppfvarnum.ForlopsID = forlopsoversikt.ForlopsID
WHERE
    aortaklaffoppfvarnum.BasisProsedyreDato >= '", fromDate, "' AND
    aortaklaffoppfvarnum.BasisProsedyreDato <= '", toDate, "'"
  )
  
  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for aortaklaffoppfvarnum"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for aortaklaffoppfvarnum"
  }
  
  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  aKoppf <- rapbase::loadRegData(registryName, query)
  
  
  
  list(aKoppf = aKoppf)
}


#' @rdname getData
#' @export
getAnD <- function(registryName, fromDate, toDate, singleRow,
                   singleHospital = NULL, ...) {
  
  # SQL possible for defined time-interval:
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- noric::getLatestEntry(registryName)
  }
  
  # Ask for all variables from annendiagnostikkvar in time interval
  # Add selected variables from forlopsoversikt
  # 2 variables to match on: AvdRESH, ForlopsID
  
  query <- paste0("
SELECT
    annendiagnostikkvarnum.*,
    forlopsoversikt.PasientID,
    forlopsoversikt.Kommune,
    forlopsoversikt.KommuneNr,
    forlopsoversikt.Fylke,
    forlopsoversikt.Fylkenr,
    forlopsoversikt.PasientAlder,
    forlopsoversikt.ForlopsType1,
    forlopsoversikt.ForlopsType2,
    forlopsoversikt.KobletForlopsID
FROM
    annendiagnostikkvarnum
LEFT JOIN forlopsoversikt ON
    annendiagnostikkvarnum.AvdRESH = forlopsoversikt.AvdRESH AND
    annendiagnostikkvarnum.ForlopsID = forlopsoversikt.ForlopsID
WHERE
    annendiagnostikkvarnum.ProsedyreDato >= '", fromDate, "' AND
    annendiagnostikkvarnum.ProsedyreDato <= '", toDate, "'"
  )
  
  if(!is.null(singleHospital)) {
    query <- paste0(query, 
                    "AND annendiagnostikkvarnum.AvdRESH = ", 
                    singleHospital)
  }
  
  
  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for annendiagnostikkvarnum"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for annendiagnostikkvarnum"
  }
  
  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  anDnum <- rapbase::loadRegData(registryName, query)
  anD <- noric::erstatt_koder_m_etiketter(anDnum,
                                          mapping = noric::ADVN_map_num_tekst)
  
  
  
  list(anD = anD)
}


#' @rdname getData
#' @export
getSs <- function(registryName, fromDate, toDate, singleRow, 
                  singleHospital = NULL, ...) {
  
  
  # SQL possible for defined time-interval:
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- noric::getLatestEntry(registryName)
  }
  
  # Ask for all variables from segmentstent in time interval
  # Add selected variables from forlopsoversikt
  # 2 variables to match on: AvdRESH, ForlopsID
  
  query <- paste0("
SELECT
    segmentstentnum.*,
    forlopsoversikt.PasientID,
    forlopsoversikt.Kommune,
    forlopsoversikt.KommuneNr,
    forlopsoversikt.Fylke,
    forlopsoversikt.Fylkenr,
    forlopsoversikt.PasientAlder,
    forlopsoversikt.ForlopsType1,
    forlopsoversikt.ForlopsType2,
    forlopsoversikt.KobletForlopsID
FROM
    segmentstentnum
LEFT JOIN forlopsoversikt ON
    segmentstentnum.AvdRESH = forlopsoversikt.AvdRESH AND
    segmentstentnum.ForlopsID = forlopsoversikt.ForlopsID
WHERE
    segmentstentnum.ProsedyreDato >= '", fromDate, "' AND
    segmentstentnum.ProsedyreDato <= '", toDate, "'"
  )
  
  if(!is.null(singleHospital)) {
    query <- paste0(query, 
                    "AND segmentstentnum.AvdRESH = ", 
                    singleHospital)
  }
  
  
  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for segmentstentnum"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for segmentstentnum"
  }
  
  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  sSnum <- rapbase::loadRegData(registryName, query)
  sS <- noric::erstatt_koder_m_etiketter(sSnum,
                                         mapping = noric::segm_map_num_tekst)
  
  
  list(sS = sS)
}




#' #' @rdname getData
#' #' @export
#' getSh <- function(registryName, fromDate, toDate, singleRow, 
#'                   singleHospital = NULL, ...) {
#'   
#'   
#'   # SQL possible for defined time-interval:
#'   if (is.null(fromDate)) {
#'     fromDate <- as.Date("1900-01-01")
#'   }
#'   if (is.null(toDate)) {
#'     toDate <- noric::getLatestEntry(registryName)
#'   }
#'   
#'   # Ask for all variables from segment_history in time interval
#'   # Add selected variables from forlopsoversikt
#'   # 2 variables to match on: AvdRESH, ForlopsID
#'   
#'   query <- paste0("
#' SELECT
#'     segment_history.*,
#'     forlopsoversikt.PasientID,
#'     forlopsoversikt.Kommune,
#'     forlopsoversikt.KommuneNr,
#'     forlopsoversikt.Fylke,
#'     forlopsoversikt.Fylkenr,
#'     forlopsoversikt.PasientAlder,
#'     forlopsoversikt.ForlopsType1,
#'     forlopsoversikt.ForlopsType2,
#'     forlopsoversikt.KobletForlopsID
#' FROM
#'     segment_history
#' LEFT JOIN forlopsoversikt ON
#'     segment_history.CENTRE_ID = forlopsoversikt.AvdRESH AND
#'     segment_history.MCEID = forlopsoversikt.ForlopsID
#' WHERE
#'     segment_history.ORGINTERDAT >= '", fromDate, "' AND
#'     segment_history.ORGINTERDAT <= '", toDate, "'"
#'   )
#'   
#'   if(!is.null(singleHospital)) {
#'     query <- paste0(query, 
#'                     "AND segment_history.CENTRE_ID = ", 
#'                     singleHospital)
#'   }
#'   
#'   
#'   # SQL for one row only/complete table:
#'   if (singleRow) {
#'     query <- paste0(query, "\nLIMIT\n  1;")
#'     msg <- "Query single row data for segment_history"
#'   } else {
#'     query <- paste0(query, ";")
#'     msg <- "Query data for segment_history"
#'   }
#'   
#'   if ("session" %in% names(list(...))) {
#'     rapbase::repLogger(session = list(...)[["session"]], msg = msg)
#'   }
#'   
#'   sH <- rapbase::loadRegData(registryName, query)
#'   
#'   
#'   list(sH = sH)
#' }



#' @rdname getData
#' @export
getMk <- function(registryName, fromDate, toDate, singleRow, ...){
  
  # SQL possible for defined time-interval:
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- noric::getLatestEntry(registryName)
  }
  
  # Ask for all variables from mitralklaff in time interval
  # Add selected variables from forlopsoversikt
  # 2 variables to match on: AvdRESH, ForlopsID
  
  query <- paste0("
SELECT
    mitralklaffvarnum.*,
    forlopsoversikt.Sykehusnavn,
    forlopsoversikt.PasientID,
    forlopsoversikt.FodselsDato,
    forlopsoversikt.Kommune,
    forlopsoversikt.KommuneNr,
    forlopsoversikt.Fylke,
    forlopsoversikt.Fylkenr,
    forlopsoversikt.PasientKjonn,
    forlopsoversikt.PasientAlder,
    forlopsoversikt.ForlopsType1,
    forlopsoversikt.ForlopsType2,
    forlopsoversikt.KobletForlopsID,
    forlopsoversikt.BasisRegStatus, 
    forlopsoversikt.Avdod,
    forlopsoversikt.AvdodDato

FROM
    mitralklaffvarnum
LEFT JOIN forlopsoversikt ON
    mitralklaffvarnum.AvdRESH = forlopsoversikt.AvdRESH AND
    mitralklaffvarnum.ForlopsID = forlopsoversikt.ForlopsID
WHERE
    mitralklaffvarnum.ProsedyreDato >= '", fromDate, "' AND
    mitralklaffvarnum.ProsedyreDato <= '", toDate, "'"
  )
  
  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for mitralklaffvarnum"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for mitralklaffvarnum"
  }
  
  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  mKnum <- rapbase::loadRegData(registryName, query)
  mK <- noric::erstatt_koder_m_etiketter(mKnum,
                                         mapping = noric::mitr_map_num_tekst)
  
  
  list(mK = mK)
}


#' @rdname getData
#' @export
getPs <- function(registryName, fromDate, toDate, singleRow, ...){
  
  # SQL possible for defined time-interval:
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- noric::getLatestEntry(registryName)
  }
  
  # Ask for all variables from pasienterstudier in time interval
  # Add selected variables from forlopsoversikt
  # 2 variables to match on: AvdRESH, ForlopsID
  
  query <- paste0("
SELECT
    pasienterstudier.*,
    forlopsoversikt.Sykehusnavn,
    forlopsoversikt.FodselsDato,
    forlopsoversikt.Kommune,
    forlopsoversikt.KommuneNr,
    forlopsoversikt.Fylke,
    forlopsoversikt.Fylkenr,
    forlopsoversikt.PasientKjonn,
    forlopsoversikt.PasientAlder

FROM
    pasienterstudier
LEFT JOIN forlopsoversikt ON
    pasienterstudier.AvdRESH = forlopsoversikt.AvdRESH AND
    pasienterstudier.PasientID = forlopsoversikt.PasientID
WHERE
    pasienterstudier.PasInklDato >= '", fromDate, "' AND
    pasienterstudier.PasInklDato <= '", toDate, "'"
  )
  
  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for pasienterstudier"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for pasienterstudier"
  }
  
  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  pS <- rapbase::loadRegData(registryName, query)
  
  
  
  list(pS = pS)
}


#' @rdname getData
#' @export
getApLight <- function(registryName, fromDate, toDate, singleRow, ...) {
  
  
  # SQL possible for defined time-interval
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- noric::getLatestEntry(registryName)
  }
  
  
  # QUERY ANGIO PCI + FO
  # Ask for all variables from ANGIO PCI in time interval
  # Add selected variables from forlopsoversikt
  # 3 variables to match on: AvdRESH, PasientID, ForlopsID
  
  query <- paste0("
SELECT
    a.*,
    f.Kommune,
    f.KommuneNr,
    f.Fylke,
    f.Fylkenr,
    f.PasientAlder,
    f.KobletForlopsID
FROM
    angiopcinum a
LEFT JOIN forlopsoversikt f ON
    a.AvdRESH = f.AvdRESH AND
    a.PasientID = f.PasientID AND
    a.ForlopsID = f.ForlopsID
WHERE
    a.ProsedyreDato >= '", fromDate, "' AND
    a.ProsedyreDato <= '", toDate, "'"
  )
  
  
  
  # Only ask for variables needed in functions:utlede_segment_stent_variabler.R
  querySs <- paste0("
SELECT
    ForlopsID, AvdRESH, StentType, Segment, Graft, ProsedyreType
FROM
    segmentstentnum
WHERE
    ProsedyreDato >= '", fromDate, "' AND
    ProsedyreDato <= '", toDate, "'
")
  
  
  # Only ask for variables needed in functions: utlede_annen_diag_variabler.R
  queryAd <- paste0("
SELECT
    ForlopsID, AvdRESH, metode
FROM
    annendiagnostikkvarnum
WHERE
    ProsedyreDato >= '", fromDate, "' AND
    ProsedyreDato <= '", toDate, "'
")
  
  
  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    querySs <- paste0(querySs, "\nLIMIT\n  1;")
    queryAd <- paste0(queryAd, "\nLIMIT\n  1;")
    msg <- "Query single row data for AngioPCI light"
  } else {
    query <- paste0(query, ";")
    querySs <- paste0(querySs, ";")
    queryAd <- paste0(queryAd, ";")
    msg <- "Query data for AngioPCI light"
  }
  
  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  aPnum <- rapbase::loadRegData(registryName, query)
  aP <- noric::erstatt_koder_m_etiketter(aPnum)
  
  aD <- rapbase::loadRegData(registryName, queryAd)
  
  sS <- rapbase::loadRegData(registryName, querySs)
  
  
  
  list(aP = aP,
       aD = aD,
       sS = sS)
}




#' @rdname getData
#' @export
getTaviProm <- function(registryName, fromDate, toDate, singleRow, ...){
  
  # SQL possible for defined time-interval:
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- noric::getLatestEntry(registryName)
  }
  
  
  queryAk <- paste0("
SELECT
    aortaklaffvarnum.DodsdatoFReg,
    aortaklaffvarnum.UtskrevetTil,
    aortaklaffvarnum.TypeKlaffeprotese,
    aortaklaffvarnum.Prosedyre,
    aortaklaffvarnum.ScreeningBeslutning,
    aortaklaffvarnum.ProsedyreDato,
    aortaklaffvarnum.FnrType, 
    forlopsoversikt.PasientID,
    forlopsoversikt.PasientKjonn,
    forlopsoversikt.PasientAlder,
    forlopsoversikt.Avdod, 
    forlopsoversikt.AvdRESH, 
    forlopsoversikt.ForlopsID
FROM
    aortaklaffvarnum
LEFT JOIN forlopsoversikt ON
    aortaklaffvarnum.AvdRESH = forlopsoversikt.AvdRESH AND
    aortaklaffvarnum.ForlopsID = forlopsoversikt.ForlopsID
WHERE
    ProsedyreDato >= '", fromDate, "' AND
    ProsedyreDato <= '", toDate, "'"
  )
  
  
  # Ask for all variables from PROM
  queryProm <- paste0("
SELECT
    *
FROM
    taviprom
WHERE
    ProsedyreDato >= '", fromDate, "' AND
    ProsedyreDato <= '", toDate, "'

 ")
  
  
  # SQL for one row only/complete table:
  if (singleRow) {
    queryProm <- paste0(queryProm, "\nLIMIT\n  1;")
    queryAk <- paste0(queryAk, "\nLIMIT\n  1;")
    msg <- "Query single row data for taviprom"
  } else {
    queryProm <- paste0(queryProm, ";")
    queryAk <- paste0(queryAk, ";")
    msg <- "Query data for taviprom"
  }
  
  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  taviProm <- rapbase::loadRegData(registryName, queryProm)
  aKnum <- rapbase::loadRegData(registryName, queryAk)
  aK <- noric::erstatt_koder_m_etiketter(aKnum,
                                         mapping = noric::aort_map_num_tekst)
  
  
  list(taviProm = taviProm, 
       aK = aK)
}