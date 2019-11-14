#' Provide registry daatabase metadata
#' 
#' List all tables and fields with attributes such as type and degault values
#'
#' @param registryName String providing the registry name key
#'
#' @return A list with table names and corresponding fields with attributes
#' @export
#'
#' @examples
#' describeRegistryDb("rapbase")

describeRegistryDb <- function(registryName) {
  
  qGetTabs <- "SHOW TABLES;"
  qGetDesc <- "DESCRIBE "
  
  desc <- list()
  
  tabs <- rapbase::LoadRegData(registryName = registryName,
                               query = qGetTabs)[[1]]
  
  for (tab in tabs) {
    query <- paste0(qGetDesc, tab, ";")
    desc[[tab]] <- rapbase::LoadRegData(registryName, query)
  }
  
  desc
}
