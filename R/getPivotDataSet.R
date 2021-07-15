#' Title
#'
#' @param setId String defining name of data set to be returned
#' @param registryName String naming the registry data
#' @param singleRow Logical defining if only one row is to be returned. A
#' relevant usecase will be when only description is needed. By default set to
#' FALSE
#' @param session List shiny session object
#' @param userRole String naming the user role
#'
#' @return data frame
#' @export
#'

getPivotDataSet <- function(setId = "", registryName, singleRow = FALSE,
                            session, userRole) {

  # declare dot
  . <- ""

  validSetId <- c("ApLight", "AnP", "AnD", "AP", "AK", "AKOppf", "CT", "FO",
                  "MK", "PS", "SO", "SS")

  if (setId %in% validSetId) {
    if (setId == "ApLight") {
      dat <- noric::getAPDataLight(registryName, singleRow = singleRow,
                                    session = session)
    }

    if (setId == "AnP") {
      dat <- noric::getLocalAnPData(registryName, singleRow = singleRow,
                                    session = session)
    }
    if (setId == "AnD") {
      dat <- noric::getAnDData(registryName, singleRow = singleRow,
                              session = session)
    }
    if (setId == "AP") {
      dat <- noric::getLocalAPData(registryName, singleRow = singleRow,
                                   session = session)
    }
    if (setId == "AK") {
      dat <- noric::getAKData(registryName, singleRow = singleRow,
                              session = session)
    }
    if (setId == "AKOppf") {
      dat <- noric::getAKOppfData(registryName, singleRow = singleRow,
                              session = session)
    }
    if (setId == "CT") {
      dat <- noric::getLocalCTData(registryName, singleRow = singleRow,
                                   session = session)
    }
    if (setId == "FO") {
      dat <- noric::getFOData(registryName, singleRow = singleRow,
                              session = session)
    }
    if (setId == "MK") {
      dat <- noric::getMKData(registryName, singleRow = singleRow,
                              session = session)
    }
    if (setId == "PS") {
      dat <- noric::getPSData(registryName, singleRow = singleRow,
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
  } else {
    dat <- NULL
  }



  #Fjerner variablene som ikke skal vises for LC
  if (userRole == "LC" && !is.null(dat)) {
    dat %<>% dplyr::select_if(!names(.) %in% c("AndreProsOperatorer",
                                               "FodselsDato",
                                               "AngioOperatorer",
                                               "PCIOperatorer",
                                               "Operatorer",
                                               "Studie",
                                               "Granskere",
                                               "OpprettetAv",
                                               "SistLagretAv"))
  }

  dat

}
