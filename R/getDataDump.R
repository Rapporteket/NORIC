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
                             "SegmentStent"))


  if (tableName %in% "AngioPCIVar"){
    tab <- noric::getAp(registryName = registryName,
                        fromDate = fromDate,
                        toDate = toDate,
                        singleRow = FALSE)
  }

  if (tableName %in% "SkjemaOversikt") {
    tab <- noric::getSo(registryName = registryName,
                        fromDate = fromDate,
                        toDate = toDate,
                        singleRow = FALSE)

  }

  if (tableName %in% "AortaklaffVar") {
    tab <- noric::getAk(registryName = registryName,
                        fromDate = fromDate,
                        toDate = toDate,
                        singleRow = FALSE)

  }

  if (tableName %in% "ForlopsOversikt") {
    tab <- noric::getFo(registryName = registryName,
                        fromDate = fromDate,
                        toDate = toDate,
                        singleRow = FALSE)

  }

  if (tableName %in% "AndreProsedyrerVar") {
    tab <- noric::getAnP(registryName = registryName,
                         fromDate = fromDate,
                         toDate = toDate,
                         singleRow = FALSE)

  }

  if (tableName %in% "CTAngioVar") {
    tab <- noric::getCt(registryName = registryName,
                        fromDate = fromDate,
                        toDate = toDate,
                        singleRow = FALSE)

  }

  if (tableName %in% "AortaklaffOppfVar") {
    tab <- noric::getAkOppf(registryName = registryName,
                            fromDate = fromDate,
                            toDate = toDate,
                            singleRow = FALSE)

  }

  if (tableName %in% "AnnenDiagnostikkVar") {
    tab <- noric::getAnD(registryName = registryName,
                         fromDate = fromDate,
                         toDate = toDate,
                         singleRow = FALSE)

  }

  if (tableName %in% "SegmentStent") {
    tab <- noric::getSs(registryName = registryName,
                        fromDate = fromDate,
                        toDate = toDate,
                        singleRow = FALSE)
  }

  tab

}
