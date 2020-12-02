#' Get hospital name from registry data
#'
#' Based on the hospital id (reshID) this function will return the name of
#' the corresponding hospital as provided in the registry data
#'
#' @param reshID string defining the resh ID
#'
#' @return string of hospital name
#' @export
#'
#' @examples
#' \dontrun{
#' getHospitalName("123456")
#' }

getHospitalName <- function(reshID) {

  if (isNationalReg(reshID)) {
    return("NORIC nasjonal")
  } else {
    baseName <- "noricStaging"
    regName <- noric::makeRegistryName(baseName, reshID)
    dbType <- "mysql"
    query <- "SELECT Sykehusnavn FROM AngioPCIVar LIMIT 1"

    return(rapbase::loadRegData(regName, dbType = dbType, query = query)[1, 1])
  }

}