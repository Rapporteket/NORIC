#' Title
#'
#' @param setId String definig name of data set to be returned
#' @param registryName String naming the registry data
#' @param session List shiny session object
#'
#' @return data frame
#' @export
#'

getPivotDataSet <- function(setId = "", registryName, session) {
  
  validSetId <- c("AnP", "AP", "SO", "SS", "CT", "AK")
  
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
    if (setId == "AK") {
      dat <- noric::getAKData(registryName, session = session)
    }
  } else {
    dat <- NULL
  }
  
  dat

}