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
    a1.*,
    a2.SEGMENT1,
    a2.SEGMENT2,
    a2.SEGMENT3,
    a2.SEGMENT4,
    a2.SEGMENT5,
    a2.SEGMENT6,
    a2.SEGMENT7,
    a2.SEGMENT8,
    a2.SEGMENT9,
    a2.SEGMENT10,
    a2.SEGMENT11,
    a2.SEGMENT12,
    a2.SEGMENT13,
    a2.SEGMENT14,
    a2.SEGMENT15,
    a2.SEGMENT16,
    a2.SEGMENT17,
    a2.SEGMENT18,
    a2.SEGMENT19,
    a2.SEGMENT20,
    a2.SEGMENT1VeneStenosgrad,
    a2.SEGMENT2VeneStenosgrad,
    a2.SEGMENT3VeneStenosgrad,
    a2.SEGMENT4VeneStenosgrad,
    a2.SEGMENT5VeneStenosgrad,
    a2.SEGMENT6VeneStenosgrad,
    a2.SEGMENT7VeneStenosgrad,
    a2.SEGMENT8VeneStenosgrad,
    a2.SEGMENT9VeneStenosgrad,
    a2.SEGMENT10VeneStenosgrad,
    a2.SEGMENT11VeneStenosgrad,
    a2.SEGMENT12VeneStenosgrad,
    a2.SEGMENT13VeneStenosgrad,
    a2.SEGMENT14VeneStenosgrad,
    a2.SEGMENT15VeneStenosgrad,
    a2.SEGMENT16VeneStenosgrad,
    a2.SEGMENT17VeneStenosgrad,
    a2.SEGMENT18VeneStenosgrad,
    a2.SEGMENT19VeneStenosgrad,
    a2.SEGMENT20VeneStenosgrad,
    a2.SEGMENT1ArterieStenosgrad,
    a2.SEGMENT2ArterieStenosgrad,
    a2.SEGMENT3ArterieStenosgrad,
    a2.SEGMENT4ArterieStenosgrad,
    a2.SEGMENT5ArterieStenosgrad,
    a2.SEGMENT6ArterieStenosgrad,
    a2.SEGMENT7ArterieStenosgrad,
    a2.SEGMENT8ArterieStenosgrad,
    a2.SEGMENT9ArterieStenosgrad,
    a2.SEGMENT10ArterieStenosgrad,
    a2.SEGMENT11ArterieStenosgrad,
    a2.SEGMENT12ArterieStenosgrad,
    a2.SEGMENT13ArterieStenosgrad,
    a2.SEGMENT14ArterieStenosgrad,
    a2.SEGMENT15ArterieStenosgrad,
    a2.SEGMENT16ArterieStenosgrad,
    a2.SEGMENT17ArterieStenosgrad,
    a2.SEGMENT18ArterieStenosgrad,
    a2.SEGMENT19ArterieStenosgrad,
    a2.SEGMENT20ArterieStenosgrad,
    a2.SekundaerBeslutning,
    a2.KomplettRevaskularisering,
    a2.AntitrombotiskFor,
    a2.TrombolyseFor,
    a2.ASAFor,
    a2.ClopidogrelFor,
    a2.PrasugrelFor,
    a2.TicagrelorFor,
    a2.HeparinFor,
    a2.DalteparinFor,
    a2.EnoxaparinFor,
    a2.AnnetLavmolHeparinFor,
    a2.BivalirudinFor,
    a2.FondaparinuxFor,
    a2.AbciximabFor,
    a2.EptifibatidFor,
    a2.TirofibanFor,
    a2.WarfarinFor,
    a2.DabigatranFor,
    a2.ApiksabanFor,
    a2.RivaroksabanFor,
    a2.EdoksabanFor,
    a2.KangrelorFor,
    a2.AnnetAntitrombotiskFor,
    a2.AntitrombotiskUnder,
    a2.TrombolyseUnder,
    a2.ASAUnder,
    a2.ClopidogrelUnder,
    a2.PrasugrelUnder,
    a2.TicagrelorUnder,
    a2.HeparinUnder,
    a2.DalteparinUnder,
    a2.EnoxaparinUnder,
    a2.AnnetLavmolHeparinUnder,
    a2.BivalirudinUnder,
    a2.FondaparinuxUnder,
    a2.AbciximabUnder,
    a2.EptifibatidUnder,
    a2.TirofibanUnder,
    a2.WarfarinUnder,
    a2.KangrelorUnder,
    a2.AnnetAntitrombotiskUnder,
    a2.PCIHovedOperator,
    a2.PCIAndreOperator,
    a2.PCITredjeOperator,
    a2.Angiografor1,
    a2.Angiografor2,
    a2.Angiografor3,
    a2.PCIOperatorer,
    a2.AngioOperatorer,
    a2.AvdKomp,
    a2.AvdKompAllergisk,
    a2.AvdKompBlodning,
    a2.AvdKompBlodningMajor,
    a2.AvdKompBlodningMinor,
    a2.AvdKompPseudoaneurysme,
    a2.AvdKompHematomStor,
    a2.AvdKompHbFallStor,
    a2.AvdKompForlengetTidStor,
    a2.AvdKompForlengetOppholdStor,
    a2.AvdKompUltralydCT,
    a2.AvdKompBlodtransfusjon,
    a2.AvdKompKirurgiskBeh,
    a2.AvdKompAnnenBehUtoverKompresjon,
    a2.AvdKompTidligUtsettelse,
    a2.AvdKompVaskulaer,
    a2.AvdKompNeurologiskKomp,
    a2.AvdKompNyNyreinsuffisiens,
    a2.AvdKompTamponade,
    a2.AvdKompPCI,
    a2.AvdKompACB,
    a2.AvdKompHjerteinfarkt,
    a2.AvdKompAnnenAlvorlig,
    a2.AvdKompDod,
    a2.AvdKompProsedyrerelatertDod,
    a2.CKMBFor,
    a2.CKMBEtter,
    a2.TroponinMetFor,
    a2.TroponinVerdiFor,
    a2.TroponinMetEtter,
    a2.TroponinVerdiEtter,
    a2.Utskrivningsdato,
    a2.UtskrevetDod,
    a2.UtskrevetDodsdato,
    a2.UtskrevetTil,
    a2.ASA,
    a2.Antikoagulantia,
    a2.AndrePlatehemmere,
    a2.UtskrStatiner,
    a2.NSAID,
    a2.ACEHemmere,
    a2.A2Blokkere,
    a2.Betablokkere,
    a2.CaBlokkere,
    a2.DiabetesBehandlingInsulin,
    a2.DiabetesBehandlingPerOral,
    a2.Digitalis,
    a2.Diuretika,
    a2.Aldosteronantagonister,
    a2.OvrigeLipidsenkere,
    a2.NitroglycerinLangtid,
    a2.AnnenAlvorligSykdom,
    a2.InfarktType,
    a2.InfarktSubklasse,
    a2.UtskrDiagnoser,
    a2.Sykehusnavn,
    a2.SkjemaStatusStart,
    a2.SkjemastatusHovedskjema,
    a2.SkjemaStatusUtskrivelse,
    a2.SkjemaStatusKomplikasjoner,
    f.Kommune,
    f.KommuneNr,
    f.Fylke,
    f.Fylkenr,
    f.PasientAlder,
    f.KobletForlopsID,
    f.ForlopsType2
FROM
    angiopcivardel1 a1
LEFT JOIN angiopcivardel2 a2 ON
    a1.AvdRESH = a2.AvdRESH AND
    a1.PasientID = a2.PasientID AND
    a1.ForlopsID = a2.ForlopsID
LEFT JOIN forlopsoversikt f ON
    a1.AvdRESH = f.AvdRESH AND
    a1.PasientID = f.PasientID AND
    a1.ForlopsID = f.ForlopsID
WHERE
    a1.ProsedyreDato >= '", fromDate, "' AND
    a1.ProsedyreDato <= '", toDate, "'"
  )

  
  if(!is.null(singleHospital)) {
    query <- paste0(query, 
                    "AND a1.AvdRESH = ", 
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
  
  aP <- rapbase::loadRegData(registryName, query)
  
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
    aortaklaffvar.*,
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
    aortaklaffvar
LEFT JOIN forlopsoversikt ON
    aortaklaffvar.AvdRESH = forlopsoversikt.AvdRESH AND
    aortaklaffvar.ForlopsID = forlopsoversikt.ForlopsID
WHERE
    ProsedyreDato >= '", fromDate, "' AND
    ProsedyreDato <= '", toDate, "'"
  )
  
  
  if(!is.null(singleHospital)) {
    query <- paste0(query, 
                    "AND aortaklaffvar.AvdRESH = ", 
                    singleHospital)
  }
  
  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for aortaklaffvar"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for aortaklaffvar"
  }
  
  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  aK <- rapbase::loadRegData(registryName, query)
  
  
  
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
  # Add selected variables from ForlopsOversikt
  # 2 variables to match on: AvdRESH, ForlopsID
  
  query <- paste0("
SELECT
    andreprosedyrervar.*,
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
    andreprosedyrervar
LEFT JOIN forlopsoversikt ON
    andreprosedyrervar.AvdRESH = forlopsoversikt.AvdRESH AND
    andreprosedyrervar.ForlopsID = forlopsoversikt.ForlopsID
WHERE
    andreprosedyrervar.ProsedyreDato >= '", fromDate, "' AND
    andreprosedyrervar.ProsedyreDato <= '", toDate, "'"
  )
  
  if(!is.null(singleHospital)) {
    query <- paste0(query, 
                    "AND andreprosedyrervar.AvdRESH = ", 
                    singleHospital)
  }
  
  
  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for andreprosedyrervar"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for andreprosedyrervar"
  }
  
  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  anP <- rapbase::loadRegData(registryName, query)
  
  
  
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
    ctangiovar.*,
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
    ctangiovar
LEFT JOIN forlopsoversikt ON
    ctangiovar.AvdRESH = forlopsoversikt.AvdRESH AND
    ctangiovar.ForlopsID = forlopsoversikt.ForlopsID
WHERE
    ctangiovar.UndersokDato >= '", fromDate, "' AND
    ctangiovar.UndersokDato <= '", toDate, "'"
  )
  
  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for ctangiovar"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for ctangiovar"
  }
  
  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  cT <- rapbase::loadRegData(registryName, query)
  
  
  
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
  
  # Ask for all variables from aortaklaffoppfvar in time interval
  # Add selected variables from forlopsoversikt
  # 2 variables to match on: AvdRESH, ForlopsID
  
  query <- paste0("
SELECT
    aortaklaffoppfvar.*,
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
    aortaklaffoppfvar
LEFT JOIN forlopsoversikt ON
    aortaklaffoppfvar.AvdRESH = forlopsoversikt.AvdRESH AND
    aortaklaffoppfvar.ForlopsID = forlopsoversikt.ForlopsID
WHERE
    aortaklaffoppfvar.BasisProsedyreDato >= '", fromDate, "' AND
    aortaklaffoppfvar.BasisProsedyreDato <= '", toDate, "'"
  )
  
  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for aortaklaffoppfvar"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for aortaklaffoppfvar"
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
    annendiagnostikkvar.*,
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
    annendiagnostikkvar
LEFT JOIN forlopsoversikt ON
    annendiagnostikkvar.AvdRESH = forlopsoversikt.AvdRESH AND
    annendiagnostikkvar.ForlopsID = forlopsoversikt.ForlopsID
WHERE
    annendiagnostikkvar.ProsedyreDato >= '", fromDate, "' AND
    annendiagnostikkvar.ProsedyreDato <= '", toDate, "'"
  )
  
  if(!is.null(singleHospital)) {
    query <- paste0(query, 
                    "AND annendiagnostikkvar.AvdRESH = ", 
                    singleHospital)
  }
  
  
  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for annendiagnostikkvar"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for annendiagnostikkvar"
  }
  
  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  anD <- rapbase::loadRegData(registryName, query)
  
  
  
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
    segmentstent.*,
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
    segmentstent
LEFT JOIN forlopsoversikt ON
    segmentstent.AvdRESH = forlopsoversikt.AvdRESH AND
    segmentstent.ForlopsID = forlopsoversikt.ForlopsID
WHERE
    segmentstent.ProsedyreDato >= '", fromDate, "' AND
    segmentstent.ProsedyreDato <= '", toDate, "'"
  )
  
  if(!is.null(singleHospital)) {
    query <- paste0(query, 
                    "AND segmentstent.AvdRESH = ", 
                    singleHospital)
  }
  
  
  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for segmentstent"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for segmentstent"
  }
  
  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  sS <- rapbase::loadRegData(registryName, query)
  
  
  
  list(sS = sS)
}


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
    mitralklaffvar.*,
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
    mitralklaffvar
LEFT JOIN forlopsoversikt ON
    mitralklaffvar.AvdRESH = forlopsoversikt.AvdRESH AND
    mitralklaffvar.ForlopsID = forlopsoversikt.ForlopsID
WHERE
    mitralklaffvar.ProsedyreDato >= '", fromDate, "' AND
    mitralklaffvar.ProsedyreDato <= '", toDate, "'"
  )
  
  # SQL for one row only/complete table:
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query single row data for mitralklaffvar"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for mitralklaffvar"
  }
  
  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  mK <- rapbase::loadRegData(registryName, query)
  
  
  
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
    a1.*,
    a2.SEGMENT1,
    a2.SEGMENT2,
    a2.SEGMENT3,
    a2.SEGMENT4,
    a2.SEGMENT5,
    a2.SEGMENT6,
    a2.SEGMENT7,
    a2.SEGMENT8,
    a2.SEGMENT9,
    a2.SEGMENT10,
    a2.SEGMENT11,
    a2.SEGMENT12,
    a2.SEGMENT13,
    a2.SEGMENT14,
    a2.SEGMENT15,
    a2.SEGMENT16,
    a2.SEGMENT17,
    a2.SEGMENT18,
    a2.SEGMENT19,
    a2.SEGMENT20,
    a2.SEGMENT1VeneStenosgrad,
    a2.SEGMENT2VeneStenosgrad,
    a2.SEGMENT3VeneStenosgrad,
    a2.SEGMENT4VeneStenosgrad,
    a2.SEGMENT5VeneStenosgrad,
    a2.SEGMENT6VeneStenosgrad,
    a2.SEGMENT7VeneStenosgrad,
    a2.SEGMENT8VeneStenosgrad,
    a2.SEGMENT9VeneStenosgrad,
    a2.SEGMENT10VeneStenosgrad,
    a2.SEGMENT11VeneStenosgrad,
    a2.SEGMENT12VeneStenosgrad,
    a2.SEGMENT13VeneStenosgrad,
    a2.SEGMENT14VeneStenosgrad,
    a2.SEGMENT15VeneStenosgrad,
    a2.SEGMENT16VeneStenosgrad,
    a2.SEGMENT17VeneStenosgrad,
    a2.SEGMENT18VeneStenosgrad,
    a2.SEGMENT19VeneStenosgrad,
    a2.SEGMENT20VeneStenosgrad,
    a2.SEGMENT1ArterieStenosgrad,
    a2.SEGMENT2ArterieStenosgrad,
    a2.SEGMENT3ArterieStenosgrad,
    a2.SEGMENT4ArterieStenosgrad,
    a2.SEGMENT5ArterieStenosgrad,
    a2.SEGMENT6ArterieStenosgrad,
    a2.SEGMENT7ArterieStenosgrad,
    a2.SEGMENT8ArterieStenosgrad,
    a2.SEGMENT9ArterieStenosgrad,
    a2.SEGMENT10ArterieStenosgrad,
    a2.SEGMENT11ArterieStenosgrad,
    a2.SEGMENT12ArterieStenosgrad,
    a2.SEGMENT13ArterieStenosgrad,
    a2.SEGMENT14ArterieStenosgrad,
    a2.SEGMENT15ArterieStenosgrad,
    a2.SEGMENT16ArterieStenosgrad,
    a2.SEGMENT17ArterieStenosgrad,
    a2.SEGMENT18ArterieStenosgrad,
    a2.SEGMENT19ArterieStenosgrad,
    a2.SEGMENT20ArterieStenosgrad,
    a2.SekundaerBeslutning,
    a2.KomplettRevaskularisering,
    a2.AntitrombotiskFor,
    a2.TrombolyseFor,
    a2.ASAFor,
    a2.ClopidogrelFor,
    a2.PrasugrelFor,
    a2.TicagrelorFor,
    a2.HeparinFor,
    a2.DalteparinFor,
    a2.EnoxaparinFor,
    a2.AnnetLavmolHeparinFor,
    a2.BivalirudinFor,
    a2.FondaparinuxFor,
    a2.AbciximabFor,
    a2.EptifibatidFor,
    a2.TirofibanFor,
    a2.WarfarinFor,
    a2.DabigatranFor,
    a2.ApiksabanFor,
    a2.RivaroksabanFor,
    a2.EdoksabanFor,
    a2.KangrelorFor,
    a2.AnnetAntitrombotiskFor,
    a2.AntitrombotiskUnder,
    a2.TrombolyseUnder,
    a2.ASAUnder,
    a2.ClopidogrelUnder,
    a2.PrasugrelUnder,
    a2.TicagrelorUnder,
    a2.HeparinUnder,
    a2.DalteparinUnder,
    a2.EnoxaparinUnder,
    a2.AnnetLavmolHeparinUnder,
    a2.BivalirudinUnder,
    a2.FondaparinuxUnder,
    a2.AbciximabUnder,
    a2.EptifibatidUnder,
    a2.TirofibanUnder,
    a2.WarfarinUnder,
    a2.KangrelorUnder,
    a2.AnnetAntitrombotiskUnder,
    a2.PCIHovedOperator,
    a2.PCIAndreOperator,
    a2.PCITredjeOperator,
    a2.Angiografor1,
    a2.Angiografor2,
    a2.Angiografor3,
    a2.PCIOperatorer,
    a2.AngioOperatorer,
    a2.AvdKomp,
    a2.AvdKompAllergisk,
    a2.AvdKompBlodning,
    a2.AvdKompBlodningMajor,
    a2.AvdKompBlodningMinor,
    a2.AvdKompPseudoaneurysme,
    a2.AvdKompHematomStor,
    a2.AvdKompHbFallStor,
    a2.AvdKompForlengetTidStor,
    a2.AvdKompForlengetOppholdStor,
    a2.AvdKompUltralydCT,
    a2.AvdKompBlodtransfusjon,
    a2.AvdKompKirurgiskBeh,
    a2.AvdKompAnnenBehUtoverKompresjon,
    a2.AvdKompTidligUtsettelse,
    a2.AvdKompVaskulaer,
    a2.AvdKompNeurologiskKomp,
    a2.AvdKompNyNyreinsuffisiens,
    a2.AvdKompTamponade,
    a2.AvdKompPCI,
    a2.AvdKompACB,
    a2.AvdKompHjerteinfarkt,
    a2.AvdKompAnnenAlvorlig,
    a2.AvdKompDod,
    a2.AvdKompProsedyrerelatertDod,
    a2.CKMBFor,
    a2.CKMBEtter,
    a2.TroponinMetFor,
    a2.TroponinVerdiFor,
    a2.TroponinMetEtter,
    a2.TroponinVerdiEtter,
    a2.Utskrivningsdato,
    a2.UtskrevetDod,
    a2.UtskrevetDodsdato,
    a2.UtskrevetTil,
    a2.ASA,
    a2.Antikoagulantia,
    a2.AndrePlatehemmere,
    a2.UtskrStatiner,
    a2.NSAID,
    a2.ACEHemmere,
    a2.A2Blokkere,
    a2.Betablokkere,
    a2.CaBlokkere,
    a2.DiabetesBehandlingInsulin,
    a2.DiabetesBehandlingPerOral,
    a2.Digitalis,
    a2.Diuretika,
    a2.Aldosteronantagonister,
    a2.OvrigeLipidsenkere,
    a2.NitroglycerinLangtid,
    a2.AnnenAlvorligSykdom,
    a2.InfarktType,
    a2.InfarktSubklasse,
    a2.UtskrDiagnoser,
    a2.Sykehusnavn,
    a2.SkjemaStatusStart,
    a2.SkjemastatusHovedskjema,
    a2.SkjemaStatusUtskrivelse,
    a2.SkjemaStatusKomplikasjoner,
    f.Kommune,
    f.KommuneNr,
    f.Fylke,
    f.Fylkenr,
    f.PasientAlder,
    f.KobletForlopsID
FROM
    angiopcivardel1 a1
LEFT JOIN angiopcivardel2 a2 ON
    a1.AvdRESH = a2.AvdRESH AND
    a1.PasientID = a2.PasientID AND
    a1.ForlopsID = a2.ForlopsID
LEFT JOIN forlopsoversikt f ON
    a1.AvdRESH = f.AvdRESH AND
    a1.PasientID = f.PasientID AND
    a1.ForlopsID = f.ForlopsID
WHERE
    a1.ProsedyreDato >= '", fromDate, "' AND
    a1.ProsedyreDato <= '", toDate, "'"
  )
  
  
  
  # Only ask for variables needed in functions:utlede_segment_stent_variabler.R
  querySs <- paste0("
SELECT
    ForlopsID, AvdRESH, StentType, Segment, Graft, ProsedyreType
FROM
    segmentstent
WHERE
    ProsedyreDato >= '", fromDate, "' AND
    ProsedyreDato <= '", toDate, "'
")
  
  
  # Only ask for variables needed in functions: utlede_annen_diag_variabler.R
  queryAd <- paste0("
SELECT
    ForlopsID, AvdRESH, metode
FROM
    annendiagnostikkvar
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
  
  aP <- rapbase::loadRegData(registryName, query)
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
    aortaklaffvar.Dodsdato,
    aortaklaffvar.UtskrevetTil,
    aortaklaffvar.TypeKlaffeprotese,
    aortaklaffvar.Prosedyre,
    aortaklaffvar.ScreeningBeslutning,
    aortaklaffvar.ProsedyreDato,
    aortaklaffvar.FnrType, 
    forlopsoversikt.PasientID,
    forlopsoversikt.PasientKjonn,
    forlopsoversikt.PasientAlder,
    forlopsoversikt.Avdod, 
    forlopsoversikt.AvdRESH, 
    forlopsoversikt.ForlopsID
FROM
    aortaklaffvar
LEFT JOIN forlopsoversikt ON
    aortaklaffvar.AvdRESH = forlopsoversikt.AvdRESH AND
    aortaklaffvar.ForlopsID = forlopsoversikt.ForlopsID
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
  aK <- rapbase::loadRegData(registryName, queryAk)
  
  
  
  list(taviProm = taviProm, 
       aK = aK)
}