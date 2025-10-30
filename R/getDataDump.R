#' Query data for dumps
#'
#' @param registryName String registry name
#' @param tableName String with name of table to query from
#' @param fromDate Character string of format YYYY-MM-DD with start date. Value
#' NULL if no filter on date.
#' @param toDate Character string of format YYYY-MM-DD with end date. Value
#' NULL if no filter on date.
#' @param ... Additional parameters to be passed to the function
#' @return A data frame with registry data
#' @export

getDataDump <- function(registryName, tableName, fromDate, toDate, ...) {
  
  stopifnot(tableName %in% c("AngioPCIVar",
                             "SkjemaOversikt",
                             "AortaklaffVar",
                             "ForlopsOversikt",
                             "AndreProsedyrerVar",
                             "CTAngioVar",
                             "AortaklaffOppfVar",
                             "AnnenDiagnostikkVar",
                             "SegmentStent",
                             "segment_history",
                             "MitralklaffVar",
                             "PasienterStudier", 
                             "AortaklaffProm", 
                             "UtskrDiagnoser"))
  
  
  if (tableName %in% "AngioPCIVar"){
    noric::getAp(registryName = registryName,
                 fromDate = fromDate,
                 toDate = toDate,
                 singleRow = FALSE)$aP
  }
  
  if (tableName %in% "SkjemaOversikt") {
    noric::getSo(registryName = registryName,
                 fromDate = fromDate,
                 toDate = toDate,
                 singleRow = FALSE)$sO
  }
  
  if (tableName %in% "AortaklaffVar") {
    noric::getAk(registryName = registryName,
                 fromDate = fromDate,
                 toDate = toDate,
                 singleRow = FALSE)$aK
  }
  
  if (tableName %in% "ForlopsOversikt") {
    noric::getFo(registryName = registryName,
                 fromDate = fromDate,
                 toDate = toDate,
                 singleRow = FALSE)$fO
  }
  
  if (tableName %in% "AndreProsedyrerVar") {
    noric::getAnP(registryName = registryName,
                  fromDate = fromDate,
                  toDate = toDate,
                  singleRow = FALSE)$anP
  }
  
  if (tableName %in% "CTAngioVar") {
    noric::getCt(registryName = registryName,
                 fromDate = fromDate,
                 toDate = toDate,
                 singleRow = FALSE)$cT
  }
  
  if (tableName %in% "AortaklaffOppfVar") {
    noric::getAkOppf(registryName = registryName,
                     fromDate = fromDate,
                     toDate = toDate,
                     singleRow = FALSE)$aKoppf
  }
  
  if (tableName %in% "AnnenDiagnostikkVar") {
    noric::getAnD(registryName = registryName,
                  fromDate = fromDate,
                  toDate = toDate,
                  singleRow = FALSE)$anD
  }
  
  if (tableName %in% "SegmentStent") {
    noric::getSs(registryName = registryName,
                 fromDate = fromDate,
                 toDate = toDate,
                 singleRow = FALSE)$sS
  }
  
  
  if (tableName %in% "MitralklaffVar") {
    noric::getMk(registryName = registryName,
                 fromDate = fromDate,
                 toDate = toDate,
                 singleRow = FALSE)$mK
  }
  
  if (tableName %in% "PasienterStudier") {
    noric::getPs(registryName = registryName,
                 fromDate = fromDate,
                 toDate = toDate,
                 singleRow = FALSE)$pS
  }
  
  
  if (tableName %in% "AortaklaffProm") {
    noric::getTaviProm(registryName = registryName,
                       fromDate = fromDate,
                       toDate = toDate,
                       singleRow = FALSE)$taviProm
  }
  
  if (tableName %in% "UtskrDiagnoser") {
    noric::getDk(registryName = registryName,
                 fromDate = fromDate,
                 toDate = toDate,
                 singleRow = FALSE)$dK
  }
  
  if (tableName %in% "segment_history") {
    noric::getSh(registryName = registryName,
                 fromDate = fromDate,
                 toDate = toDate,
                 singleRow = FALSE)$sH
  }
}
