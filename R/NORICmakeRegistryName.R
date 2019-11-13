#' Provide registry name corresponding to db config entry
#'
#' Provides the registry name key to be used for reading correspponding
#' entries from the (yaml) configuration file
#'
#' @param baseName String giving the prefix base of the name
#' @param reshID String providing the current reshID. At Rapporteket, reshID
#' should already be present in the current R session
#' @param role String defining the current role of the user. Defaults to "LU"
#' @param localRegistry Logical that if TRUE (default) make the function return
#' a registry name corresponing to the local registry. If FALSE the function
#' will try to return the name of the national registry falling back to the
#' local registry name if not successfully so 
#' @return registryName String containing the registry name as used in config
#' @export

NORICmakeRegistryName <- function(baseName, reshID=reshID, role = "LU",
                                  localRegistry = TRUE) {
  
  if (localRegistry) {
    return(paste0(baseName, reshID))
  } else {
    conf <- rapbase::getConfig(fileName = "rapbaseConfig.yml")
    if (reshID == conf$reg$noric$nationalAccess$reshId &&
        role == conf$reg$noric$nationalAccess$userRole) {
      return(paste0(baseName, conf$reg$noric$nationalAccess$nameKey))
    } else {
      warning("Someting is fishy! Falling back to local registry name")
      return(paste0(baseName, reshID))
    }
  }
}