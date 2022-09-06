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
                             "MitralklaffVar",
                             "PasienterStudier", 
                             "PromVar"))
  
  
  if (tableName %in% "AngioPCIVar"){
    tab_list <- noric::getAp(registryName = registryName,
                             fromDate = fromDate,
                             toDate = toDate,
                             singleRow = FALSE)
    tab <- tab_list$aP
  }
  
  if (tableName %in% "SkjemaOversikt") {
    tab_list <- noric::getSo(registryName = registryName,
                             fromDate = fromDate,
                             toDate = toDate,
                             singleRow = FALSE)
    tab <- tab_list$sO
  }
  
  if (tableName %in% "AortaklaffVar") {
    tab_list <- noric::getAk(registryName = registryName,
                             fromDate = fromDate,
                             toDate = toDate,
                             singleRow = FALSE)
    tab <- tab_list$aK
  }
  
  if (tableName %in% "ForlopsOversikt") {
    tab_list <- noric::getFo(registryName = registryName,
                             fromDate = fromDate,
                             toDate = toDate,
                             singleRow = FALSE)
    tab <- tab_list$fO
  }
  
  if (tableName %in% "AndreProsedyrerVar") {
    tab_list <- noric::getAnP(registryName = registryName,
                              fromDate = fromDate,
                              toDate = toDate,
                              singleRow = FALSE)
    tab <- tab_list$anP
  }
  
  if (tableName %in% "CTAngioVar") {
    tab_list <- noric::getCt(registryName = registryName,
                             fromDate = fromDate,
                             toDate = toDate,
                             singleRow = FALSE)
    tab <- tab_list$cT
  }
  
  if (tableName %in% "AortaklaffOppfVar") {
    tab_list <- noric::getAkOppf(registryName = registryName,
                                 fromDate = fromDate,
                                 toDate = toDate,
                                 singleRow = FALSE)
    tab <- tab_list$aKoppf
  }
  
  if (tableName %in% "AnnenDiagnostikkVar") {
    tab_list <- noric::getAnD(registryName = registryName,
                              fromDate = fromDate,
                              toDate = toDate,
                              singleRow = FALSE)
    tab <- tab_list$anD
  }
  
  if (tableName %in% "SegmentStent") {
    tab_list <- noric::getSs(registryName = registryName,
                             fromDate = fromDate,
                             toDate = toDate,
                             singleRow = FALSE)
    tab <- tab_list$sS
  }
  
  
  if (tableName %in% "MitralklaffVar") {
    tab_list <- noric::getMk(registryName = registryName,
                             fromDate = fromDate,
                             toDate = toDate,
                             singleRow = FALSE)
    tab <- tab_list$mK
  }
  
  if (tableName %in% "PasienterStudier") {
    tab_list <- noric::getPs(registryName = registryName,
                             fromDate = fromDate,
                             toDate = toDate,
                             singleRow = FALSE)
    tab <- tab_list$pS
  }
  
  
  if (tableName %in% "PromVar") {
    tab_list <- noric::getProm(registryName = registryName,
                               fromDate = fromDate,
                               toDate = toDate,
                               singleRow = FALSE)
    tab <- tab_list$prom
    
  }
  
  tab
  
}
