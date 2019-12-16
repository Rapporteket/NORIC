#' Title
#'
#' @param setId String definig name of data set to be returned
#' @param registryName String naming the registry data
#' @param singleRow Logical defining if only one row is to be returned. A
#' relevant usecase will be when only description is needed. By default set to
#' FALSE
#' @param session List shiny session object
#'
#' @return data frame
#' @export
#'

getPivotDataSet <- function(setId = "", registryName, singleRow = FALSE,
                            session) {
  
  validSetId <- c("AnP", "AP", "SO", "SS", "CT", "AK")
  
  if (setId %in% validSetId) {
    if (setId == "AnP") {
      dat <- noric::getLocalAnPData(registryName, singleRow = singleRow,
                                    session = session)
    } 
    if (setId == "AP") {
      dat <- noric::getLocalAPData(registryName, singleRow = singleRow,
                                   session = session)
    }
    if (setId == "SO") {
      dat <- noric::getLocalSOData(registryName, singleRow = singleRow,
                                   session = session)
    }
    if (setId == "SS") {
      dat <- noric::getLocalSSData(registryName, singleRow = singleRow,
                                   session = session)
    }
    if (setId == "CT") {
      dat <- noric::getLocalCTData(registryName, singleRow = singleRow,
                                   session = session)
    }
    if (setId == "AK") {
      dat <- noric::getAKData(registryName, singleRow = singleRow,
                              session = session)
    }
  } else {
    dat <- NULL
  }
  
  dat

}