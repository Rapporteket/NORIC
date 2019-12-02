#' Provide registry daatabase metadata
#' 
#' List all tables and fields with attributes such as type and degault values
#'
#' @param registryName String providing the registry name key
#' @param tabs Character vector for optional definition of tables to describe.
#' Defaults to an empty vector in which case all tables are used
#'
#' @return A list with table names and corresponding fields with attributes
#' @export
#'
#' @examples
#' describeRegistryDb("rapbase")

describeRegistryDb <- function(registryName, tabs = c()) {
  
  qGetTabs <- "SHOW TABLES;"
  qGetDesc <- "DESCRIBE "
  
  desc <- list()
  
  if (length(tabs) == 0) {
    tabs <- rapbase::LoadRegData(registryName = registryName,
                                 query = qGetTabs)[[1]]
  }
  
  for (tab in tabs) {
    query <- paste0(qGetDesc, tab, ";")
    desc[[tab]] <- rapbase::LoadRegData(registryName, query)
  }
  
  desc
}
