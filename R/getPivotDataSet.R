#' Get data set for Pivot-table
#'
#' Load pre-processed dataset into pivot table.
#'
#' @param setId String defining name of data set to be returned
#' @param registryName String naming the registry data
#' @param singleRow Logical defining if only one row is to be returned. A
#' relevant usecase will be when only description/variable-list is needed.
#' By default set to FALSE
#' @param session List shiny session object
#' @param userRole String naming the user role
#' @param fromDate first date to fetch
#' @param toDate last date to fetch
#' @param singleHospital AvdRESH to fetch data from
#'
#' @return data frame
#' @export
#'

getPivotDataSet <- function(setId = "", registryName, singleRow = FALSE,
                            session, userRole, fromDate, toDate, 
                            singleHospital) {
  
  # declare dot
  . <- ""
  
  validSetId <- c("ApLight", "AnP", "AnD", "AP", "AK", "AKOppf", "CT", "FO",
                  "MK", "PS", "SO", "SS", "TP")
  
  if (setId %in% validSetId) {
    
    if (setId == "ApLight") {
      dat <- noric::getPrepApLightData(registryName = registryName,
                                       fromDate = fromDate,
                                       toDate = toDate,
                                       singleRow = singleRow,
                                       session = session, 
                                       singleHospital = singleHospital)
    }
    
    if (setId == "AnP") {
      dat <- noric::getPrepAnPData(registryName = registryName,
                                   fromDate = fromDate,
                                   toDate = toDate,
                                   singleRow = singleRow,
                                   session = session, 
                                   singleHospital = singleHospital)
    }
    if (setId == "AnD") {
      dat <- noric::getPrepAnDData(registryName = registryName,
                                   fromDate = fromDate,
                                   toDate = toDate,
                                   singleRow = singleRow,
                                   session = session, 
                                   singleHospital = singleHospital)
    }
    if (setId == "AP") {
      dat <- noric::getPrepApData(registryName = registryName,
                                  fromDate = fromDate,
                                  toDate = toDate,
                                  singleRow = singleRow,
                                  session = session, 
                                  singleHospital = singleHospital)
    }
    if (setId == "AK") {
      dat <- noric::getPrepAkData(registryName = registryName,
                                  fromDate = fromDate,
                                  toDate = toDate,
                                  singleRow = singleRow,
                                  session = session, 
                                  singleHospital = singleHospital)
    }
    if (setId == "AKOppf") {
      dat <- noric::getPrepAkOppfData(registryName = registryName,
                                      fromDate = fromDate,
                                      toDate = toDate,
                                      singleRow = singleRow,
                                      session = session, 
                                      singleHospital = singleHospital)
    }
    if (setId == "CT") {
      dat <- noric::getPrepCtData(registryName = registryName,
                                  fromDate = fromDate,
                                  toDate = toDate,
                                  singleRow = singleRow,
                                  session = session, 
                                  singleHospital = singleHospital)
    }
    if (setId == "FO") {
      dat <- noric::getPrepFoData(registryName = registryName,
                                  fromDate = fromDate,
                                  toDate = toDate,
                                  singleRow = singleRow,
                                  session = session, 
                                  singleHospital = singleHospital)
    }
    if (setId == "MK") {
      dat <- noric::getPrepMkData(registryName = registryName,
                                  fromDate = fromDate,
                                  toDate = toDate,
                                  singleRow = singleRow,
                                  session = session, 
                                  singleHospital = singleHospital)
    }
    if (setId == "PS") {
      dat <- noric::getPrepPsData(registryName = registryName,
                                  fromDate = fromDate,
                                  toDate = toDate,
                                  singleRow = singleRow,
                                  session = session, 
                                  singleHospital = singleHospital)
    }
    if (setId == "SO") {
      dat <- noric::getPrepSoData(registryName = registryName,
                                  fromDate = fromDate,
                                  toDate = toDate,
                                  singleRow = singleRow,
                                  session = session, 
                                  singleHospital = singleHospital)
    }
    if (setId == "SS") {
      dat <- noric::getPrepSsData(registryName = registryName,
                                  fromDate = fromDate,
                                  toDate = toDate,
                                  singleRow = singleRow,
                                  session = session, 
                                  singleHospital = singleHospital)
    }
    
    if (setId == "TP") {
      dat <- noric::getPrepTaviPromData(registryName = registryName,
                                        fromDate = fromDate,
                                        toDate = toDate,
                                        singleRow = singleRow,
                                        session = session, 
                                        singleHospital = singleHospital)
    }
    
  } else {
    dat <- NULL
  }
  
  
  
  #Fjerner variablene som ikke skal vises for LC
  if (userRole == "LC" && !is.null(dat)) {
    dat %<>% dplyr::select_if(!names(.) %in% c("AndreProsOperatorer",
                                               "FodselsDato",
                                               "PCIHovedOperator",
                                               "PCIAndreOperator",
                                               "PCITredjeOperator",
                                               "Angiografor1",
                                               "Angiografor2",
                                               "Angiografor3",
                                               "PCIOperatorer",
                                               "AngioOperatorer",
                                               "Operatorer",
                                               "Studie",
                                               "Granskere",
                                               "OpprettetAv",
                                               "SistLagretAv", 
                                               "FnrType"))
  }
  
  dat
  
}
