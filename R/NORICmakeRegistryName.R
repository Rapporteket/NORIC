#' Provide registry name corresponding to db config entry
#'
#' Provides the registry name key to be used for reading correspponding
#' entries from the (yaml) configuration file
#'
#' @param baseName String giving the prefix base of the name
#' @param reshID String providing the current reshID. At Rapporteket, reshID
#' should already be present in the current R session
#' @return String containing a valid registry name
#' @export

NORICmakeRegistryName <- function(baseName, reshID=reshID) {

  if (isNationalReg(reshID)) {
    conf <- rapbase::getConfig(fileName = "rapbaseConfig.yml")
    return(paste0(baseName, conf$reg$noric$nationalAccess$nameKey))
  } else {
    return(paste0(baseName, reshID))
  }

}