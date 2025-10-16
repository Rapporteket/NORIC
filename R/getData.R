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
#' getCt
#' getAk
#' getAkOppf
#' getAnP
#' getAnD
#' getSs
#' getMk
#' getTaviProm
#' getSh
#' getFo
#' getSo
#' getPs
#' getApLight
NULL
#' @rdname getData
#' @export
getAp <- function(registryName, fromDate, toDate, singleRow,
                  singleHospital = NULL, ...) {

  if (is.null(fromDate)) {fromDate <- as.Date("1900-01-01")}
  if (is.null(toDate)) {toDate <- noric::getLatestEntry(registryName)}

  query <- paste0(noric::queryAngiopcinum(),
                  "WHERE
                  A.INTERDAT >= '", fromDate, "' AND
                  A.INTERDAT <= '", toDate, "' ")
  if(!is.null(singleHospital)) {
    query <- paste0(query, "AND A.CENTREID = ", singleHospital)
  }

  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for AngioPCI"
  } else {
    query <- paste0(query, " ;")
    msg <- "Query data for AngioPCI"
  }

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }

  aPnum <- rapbase::loadRegData(registryName, query)
  aP <- noric::erstatt_koder_m_etiketter(
    aPnum,
    mapping = noric::angp_map_num_tekst) %>%
    noric::utlede_alder(., var = ProsedyreDato)%>%
    noric::fikse_sykehusnavn(.)

  list(aP = aP)
}

#' @rdname getData
#' @export
getCt <- function(registryName, fromDate, toDate, singleRow,
                  singleHospital = NULL, ...){
  
  if (is.null(fromDate)) {fromDate <- as.Date("1900-01-01")}
  if (is.null(toDate)) {toDate <- noric::getLatestEntry(registryName)}
  
  query <- paste0(noric::queryCtangiovarnum(), 
                  "AND
                  CT.CTDAT >= '", fromDate, "' AND
                  CT.CTDAT <= '", toDate, "' ")
  
  if(!is.null(singleHospital)) {
    query <- paste0(query, "AND CT.CENTREID = ", singleHospital)
  }
  
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
  cT <-  noric::erstatt_koder_m_etiketter(
    cTnum,
    mapping = noric::CTANG_map_num_tekst) %>% 
    noric::utlede_alder(var = UndersokDato)%>% 
    noric::fikse_sykehusnavn(.)
  
  
  list(cT = cT)
}



#' @rdname getData
#' @export
getAk <- function(registryName, fromDate, toDate, singleRow, 
                  singleHospital = NULL, ...){
  
  if (is.null(fromDate)) {fromDate <- as.Date("1900-01-01")}
  if (is.null(toDate)) {toDate <- noric::getLatestEntry(registryName)}
  
  query <- paste0(noric::queryAortaklaffvarnum(), 
                  "WHERE
                  T.PROCEDUREDATE >= '", fromDate, "' AND
                  T.PROCEDUREDATE <= '", toDate, "'")
  
  if(!is.null(singleHospital)) {
    query <- paste0(query, "AND T.CENTREID = ", singleHospital)
  }
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;") # single row
    msg <- "Query single row data for aortaklaffvarnum"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for aortaklaffvarnum"
  }
  
  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  aKnum <- rapbase::loadRegData(registryName, query)
  aK <- noric::erstatt_koder_m_etiketter(
    df = aKnum,
    mapping = noric::aort_map_num_tekst) %>% 
    noric::utlede_alder(., var = ProsedyreDato)%>% 
    noric::fikse_sykehusnavn(.)
  list(aK = aK)
}


#' @rdname getData
#' @export
getAkOppf <- function(registryName, fromDate, toDate, singleRow,
                      singleHospital = NULL, ...){
  
  if (is.null(fromDate)) {fromDate <- as.Date("1900-01-01")}
  if (is.null(toDate)) {toDate <- noric::getLatestEntry(registryName)}
  
  query <- paste0(noric::queryAortaklaffoppfvarnum(), 
                  "WHERE
                  T.PROCEDUREDATE >= '", fromDate, "' AND
                  T.PROCEDUREDATE <= '", toDate, "'")
  
  # NB usikker på om vi skal bruke TF eller T her. followup eller tavi
  if(!is.null(singleHospital)) {
    query <- paste0(query, "AND TF.CENTREID = ", singleHospital)
  }
  
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
  
  
  query_fo_temp <- paste0("
   SELECT
    forlopsoversikt.AvdRESH,
    forlopsoversikt.ForlopsID,
    forlopsoversikt.Sykehusnavn,
    forlopsoversikt.PasientID,
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
    forlopsoversikt;")
  
  fo_tmp <- rapbase::loadRegData(registryName, query_fo_temp)
  
  aKoppf %<>% dplyr::left_join(., 
                               fo_tmp,
                               by = c("AvdRESH", "ForlopsID"))
  
  list(aKoppf = aKoppf)
}

#' @rdname getData
#' @export
getAnP <- function(registryName, fromDate, toDate, singleRow,
                   singleHospital = NULL, ...) {                  
  
  if (is.null(fromDate)) {fromDate <- as.Date("1900-01-01")}
  if (is.null(toDate)) {toDate <- noric::getLatestEntry(registryName)}
  
  
  query <- paste0(noric::queryAndreprosedyrervarnum(), 
                  "WHERE
                  other.PROCEDUREDATE >= '", fromDate, "' AND
                  other.PROCEDUREDATE <= '", toDate, "'")
  if(!is.null(singleHospital)) {
    query <- paste0(query, 
                    "AND other.CENTREID = ", 
                    singleHospital)
  }
  
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1 ;")
    msg <- "Query single row data for andreprosedyrervarnum"
  } else {
    query <- paste0(query, " ;")
    msg <- "Query data for andreprosedyrervarnum"
  }
  
  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  anPnum <- rapbase::loadRegData(registryName, query)
  anP <- noric::erstatt_koder_m_etiketter(anPnum,
                                          mapping = noric::APVN_map_num_tekst) %>% 
    noric::utlede_alder(., var = ProsedyreDato) %>% 
    noric::fikse_sykehusnavn(.)

  list(anP = anP)
}



#' @rdname getData
#' @export
getAnD <- function(registryName, fromDate, toDate, singleRow,
                   singleHospital = NULL, ...) {
  if (is.null(fromDate)) {fromDate <- as.Date("1900-01-01")}
  if (is.null(toDate)) {toDate <- noric::getLatestEntry(registryName)}
  
  query <- paste0(noric::queryAnnendiagnostikkvarnum(), 
                  " WHERE 
                  R.INTERDAT >= '", fromDate,  "' AND 
                  R.INTERDAT <= '", toDate, "' ")
  
  if(!is.null(singleHospital)) {
    query <- paste0(query, "AND MCE.CENTREID = ", singleHospital)
  }
  
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for annendiagnostikkvarnum"
  } else {
    query <- paste0(query, " ;")
    msg <- "Query data for annendiagnostikkvarnum"
  }
  
  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  anDnum <- rapbase::loadRegData(registryName, query)
  anD <- noric::erstatt_koder_m_etiketter(anDnum,
                                          mapping = noric::ADVN_map_num_tekst) %>% 
    noric::utlede_alder(., var = ProsedyreDato) %>% 
    noric::fikse_sykehusnavn(.)
  
  list(anD = anD)
}


#' @rdname getData
#' @export
getSs <- function(registryName, fromDate, toDate, singleRow,
                  singleHospital = NULL, ...) {

  if (is.null(fromDate)) {fromDate <- as.Date("1900-01-01")}
  if (is.null(toDate)) {toDate <- noric::getLatestEntry(registryName)}

  query <- paste0(noric::querySegmentstentnum(),
                  " WHERE
                  R.INTERDAT >= '", fromDate,  "' AND
                  R.INTERDAT <= '", toDate, "' ")

  if(!is.null(singleHospital)) {
    query <- paste0(query, "AND MCE.CENTREID = ", singleHospital)
  }

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
                                         mapping = noric::segm_map_num_tekst) %>%
    noric::utlede_alder(., var = ProsedyreDato) %>%
    noric::fikse_sykehusnavn(.)

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
getMk <- function(registryName, fromDate, toDate, singleRow,
                  singleHospital = NULL, ...){

  if (is.null(fromDate)) {fromDate <- as.Date("1900-01-01")}
  if (is.null(toDate)) {toDate <- noric::getLatestEntry(registryName)}

  query <- paste0(noric::queryMitralklaffvarnum(),
                  " WHERE
                  T.PROCEDUREDATE >= '", fromDate,  "' AND
                  T.PROCEDUREDATE <= '", toDate, "' ")

  if(!is.null(singleHospital)) {
    query <- paste0(query, "AND MCE.CENTREID = ", singleHospital)
  }
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
  mK <- noric::erstatt_koder_m_etiketter(
    mKnum,
    mapping = noric::mitr_map_num_tekst) %>%
    noric::fikse_sykehusnavn(.)

  # %>%
    # noric::utlede_alder(., var = ProsedyreDato)

  list(mK = mK)
}




#' @rdname getData
#' @export
getTaviProm <- function(registryName, fromDate, toDate, singleRow, 
                        singleHospital = NULL, ...){
  
  if (is.null(fromDate)) {fromDate <- as.Date("1900-01-01")}
  if (is.null(toDate)) {toDate <- noric::getLatestEntry(registryName)}
  
  
  
  queryAk <- paste0("
  SELECT
    T.CENTREID AS AvdRESH,
    T.MCEID AS ForlopsID,
    P.ID AS PasientID, 
    T.COMPLETED_PROCEDURE AS Prosedyre,
    T.SCREENING AS ScreeningBeslutning,
    T.PROCEDUREDATE AS ProsedyreDato,
    CASE
      WHEN IFNULL(P.GENDER,0) = 0 THEN 'Ikke angitt'
      WHEN P.GENDER = 1 THEN 'Mann'
      WHEN P.GENDER = 2 THEN 'Kvinne'
      ELSE 'Ukjent'
    END AS PasientKjonn,
    P.BIRTH_DATE AS FodselsDato,
    P.SSN_TYPE AS FnrType,
    P.DECEASED AS AvdodFReg,
    P.DECEASED_DATE AS DodsdatoFReg,
    (SELECT v.NAME FROM valve v WHERE T.INSTRUMENTTYPE = v.ID) AS TypeKlaffeprotese,
    TD.DISCHARGETO AS UtskrevetTil
    
   FROM mce MCE
      INNER JOIN patient P ON MCE.PATIENT_ID = P.ID
      INNER JOIN taviperc T ON MCE.MCEID = T.MCEID
      INNER JOIN tavidischarge TD ON MCE.MCEID = TD.MCEID
 
   WHERE
    T.PROCEDUREDATE >= '", fromDate, "' AND
    T.PROCEDUREDATE <= '", toDate, "'
 ")
  

  queryProm <- paste0(
    noric::queryTaviprom(), 
    "AND
    tavi.PROCEDUREDATE >= '", fromDate, "' AND
    tavi.PROCEDUREDATE <= '", toDate, "'
    ")
  
  if(!is.null(singleHospital)) {
    queryAk <- paste0(queryAk, "AND T.CENTREID = ", singleHospital)
    queryProm <- paste0(queryProm, "AND MCE.CENTREID = ", singleHospital)
  }

  
  if (singleRow) {
    queryProm <- paste0(queryProm, "\nLIMIT\n  1;")
    queryAk <- paste0(queryAk, "\nLIMIT\n  1;")
    msg <- "Query single row data for taviprom"
  } else {
    queryProm <- paste0(queryProm, " ;")
    queryAk <- paste0(queryAk, " ;")
    msg <- "Query data for taviprom"
  }
  
  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  taviProm <- rapbase::loadRegData(registryName, queryProm)
  aKnum <- rapbase::loadRegData(registryName, queryAk)
  aK <- noric::erstatt_koder_m_etiketter(aKnum,
                                         mapping = noric::aort_map_num_tekst) %>% 
    noric::utlede_alder(df = ., var = ProsedyreDato)
  
  
  list(taviProm = taviProm, 
       aK = aK)
}



#' @rdname getData
#' @export
getFo <- function(registryName, fromDate, toDate, singleRow, 
                  singleHospital = NULL, ...) {
  
  # FO: Datoer. Blandet dato for prosedyre, oppfølging, prom. 
  # Hente hele datasettet uansett fromDate, toDate
  query <- noric::queryForlopsoversikt()
  if(!is.null(singleHospital)) {
    query <- paste0(query, 
                    "WHERE mce.CENTREID = ", 
                    singleHospital)
  }
  
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
  
  fO <- rapbase::loadRegData(registryName, query) %>% 
    noric::utlede_alder(., HovedDato) %>% 
    noric::fikse_sykehusnavn(.)
  
  list(fO = fO)
}



#' @rdname getData
#' @export
getSo <- function(registryName, fromDate, toDate, singleRow, 
                  singleHospital = NULL, ...) {
  
  if (is.null(fromDate)) {fromDate <- as.Date("1900-01-01")}
  if (is.null(toDate)) {toDate <- noric::getLatestEntry(registryName)}
  
  
  # HVA SKJER MED DENNE i ny noric_bergen?
  # query <- noric::querySkjemaovesikt(fromDate = fromDate, 
  #                                    toDate = toDate, 
  #                                    singleHospital = singleHospital)
  # 
  # if(!is.null(singleHospital)) {
  #   query <- paste0(query, "AND A.CENTREID = ", singleHospital)
  # }
  
  
  query <- paste0("SELECT * FROM  skjemaoversikt
                  WHERE HovedDato >= '", fromDate, 
                  "' AND HovedDato <= '", toDate, "' ")
  
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
getPs <- function(registryName, fromDate, toDate, singleRow, 
                  singleHospital = NULL, ...){
  
  if (is.null(fromDate)) {fromDate <- as.Date("1900-01-01")}
  if (is.null(toDate)) {toDate <- noric::getLatestEntry(registryName)}
  
  
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
getApLight <- function(registryName, fromDate, toDate, singleRow, 
                       singleHospital = NULL, ...) {
  
  if (is.null(fromDate)) {fromDate <- as.Date("1900-01-01")}
  if (is.null(toDate)) {toDate <- noric::getLatestEntry(registryName)}

  queryAp <- paste0(noric::queryApLight(), 
                  "WHERE
                  A.INTERDAT >= '", fromDate, "' AND
                  A.INTERDAT <= '", toDate, "' ")

  # Only ask for variables needed in functions: utlede_annen_diag_variabler.R 
  # and utlede_segment_stent_variabler.R
  querySs <- paste0("
      SELECT     
        mce.CENTREID AS AvdRESH,
        segment.MCEID AS ForlopsID,
        stent.DES  AS StentType,
        segment.SEGMENT as Segment,
        segment.GRAFT  AS Graft,
        segment.PROCTYP  AS ProsedyreType
      FROM segment 
      LEFT  JOIN stent  ON segment.STENT = stent.SID
      INNER JOIN mce ON segment.MCEID = mce.MCEID
      WHERE 
        mce.INTERDAT >= '", fromDate, "' AND
        mce.INTERDAT <= '", toDate, "' ")
 
  queryAd <- paste0("
      SELECT     
        mce.CENTREID AS AvdRESH,
        diagnostics.MCEID AS ForlopsID,
        diagnostics.METHODUSED  AS metode
      FROM diagnostics 
      INNER JOIN mce ON diagnostics.MCEID = mce.MCEID
      WHERE 
        mce.INTERDAT >= '", fromDate, "' AND
        mce.INTERDAT <= '", toDate, "' ")
  
  if(!is.null(singleHospital)) {
    queryAp <- paste0(queryAp, "AND A.CENTREID = ", singleHospital)
    querySs <- paste0(querySs, "AND mce.CENTREID = ", singleHospital)
    queryAd <- paste0(queryAd, "AND mce.CENTREID = ", singleHospital)
  }
  
  if (singleRow) {
    queryAp <- paste0(queryAp, "\nLIMIT\n  1;")
    querySs <- paste0(querySs, "\nLIMIT\n  1;")
    queryAd <- paste0(queryAd, "\nLIMIT\n  1;")
    msg <- "Query single row data for AngioPCI light"
  } else {
    queryAp <- paste0(queryAp, ";")
    querySs <- paste0(querySs, ";")
    queryAd <- paste0(queryAd, ";")
    msg <- "Query data for AngioPCI light"
  }
  
  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  aPnum <- rapbase::loadRegData(registryName, queryAp)
  aP <- noric::erstatt_koder_m_etiketter(aPnum)
  
  aDnum <- rapbase::loadRegData(registryName, queryAd)
  aD <- noric::erstatt_koder_m_etiketter(aDnum,
                                         mapping = noric::ADVN_map_num_tekst)
  
  sSnum <- rapbase::loadRegData(registryName, querySs)
  sS <- noric::erstatt_koder_m_etiketter(sSnum,
                                         mapping = noric::segm_map_num_tekst)
  list(aP = aP,
       aD = aD,
       sS = sS)
}


