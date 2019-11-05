#' Title
#'
#' @param setId 
#' @param registryName 
#' @param session 
#'
#' @return data frame
#' @export
#'

getPivotDataSet <- function(setId = "", registryName, session) {
  
  validSetId <- c("AnP", "AP", "SO", "SS", "CT")
  
  if (setId %in% validSetId) {
    if (setId == "AnP") {
      dat <- noric::getLocalAnPData(registryName, session = session)
    } 
    if (setId == "AP") {
      dat <- noric::getLocalAPData(registryName, session = session)
    }
    if (setId == "SO") {
      dat <- noric::getLocalSOData(registryName, session = session)
    }
    if (setId == "SS") {
      dat <- noric::getLocalSSData(registryName, session = session)
    }
    if (setId == "CT") {
      dat <- noric::getLocalCTData(registryName, session = session)
    }
  } else {
    dat <- NULL
  }
  
  dat

}