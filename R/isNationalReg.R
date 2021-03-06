#' Is organization a national registry?
#'
#' By matching organization ID and configuration, TRUE or FALSE is returned
#' if that organization represents the national registrey or not
#'
#' @param reshId Integer organization ID
#'
#' @return Logical (TRUE or FALSE)
#' @export

isNationalReg <- function(reshId) {

  conf <- rapbase::getConfig(fileName = "rapbaseConfig.yml")
  if (reshId == conf$reg$noric$nationalAccess$reshId) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
