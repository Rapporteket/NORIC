#' Provide registry name corresponding to db config entry
#'
#' Provides the registry name key to be used for reasing correspponding
#' entries from the (yaml) configuration file
#'
#' @param baseName String giving the prefix base of the name
#' @param reshID String providing the current reshID. At Rapporteket, reshID
#'  should already be present in the current R session
#' @return registryName String containing the registry name as used in config
#' @export

NORICmakeRegistryName <- function(baseName, reshID=reshID) {
  
  registryName <- paste0(baseName, reshID)
  
  return(registryName)
}