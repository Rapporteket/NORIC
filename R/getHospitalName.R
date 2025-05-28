#' Get hospital name from registry data
#'
#' Based on the hospital id (reshID) this function will return the name of
#' the corresponding hospital as provided in the registry data
#'
#' @param regName string defining the db
#' @param national true or false
#'
#' @return string of hospital name
#' @export
#'
#' @examples
#' \dontrun{
#' getHospitalName("my_db")
#' }

getHospitalName <- function(regName, national = FALSE) {

  if (national) {
    return("NORIC nasjonal")
  } else {
    dbType <- "mysql"
    query <- "SELECT Sykehusnavn FROM angiopcinum LIMIT 1"

    return(rapbase::loadRegData(registryName = regName,
                                dbType = dbType, 
                                query = query)[1, 1])
  }

}
