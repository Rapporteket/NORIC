#' Get all hospitals present in the registry databases
#'
#' Provides all AvdRESH present in AP and AK databases. Used to test
#' completness of data-transfert before creation of montly reports.
#'
#' @param registryName Character string providing registry name key
#' @param fromDate Character string of format YYYY-MM-DD with start date. Value
#' NULL if no filter on date.
#' @param toDate Character string of format YYYY-MM-DD with end date. Value
#' NULL if no filter on date.#' @return A Date object
#' @return character string with AvdRESH
#' 
#' @name getPresentHospitals
#' @aliases getPresentHospitalsAp
#' getPresentHospitalsAk
NULL
 


#' @rdname getPresentHospitals
#' @export
getPresentHospitalsAp <-function(registryName, 
                                 fromDate = NULL, 
                                 toDate = NULL){
 
  # SQL possible for defined time-interval:
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- noric::getLatestEntry(registryName)
  }
   
  query <- paste0("
    SELECT 
      DISTINCT regangio.CENTREID AS AvdRESH 
    FROM
      regangio 
    WHERE
     regangio.INTERDAT >= '", fromDate, "' AND
     regangio.INTERDAT <= '", toDate, "'", 
    ";")
  
  rapbase::loadRegData(registryName = registryName, 
                       query = query)$AvdRESH
}

#' @rdname getPresentHospitals
#' @export
getPresentHospitalsAk <-function(registryName,  
                                 fromDate = NULL, 
                                 toDate = NULL){
  
  # SQL possible for defined time-interval:
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- noric::getLatestEntry(registryName)
  }
  
  
  query <- paste0("
    SELECT 
      DISTINCT taviperc.CENTREID AS AvdRESH 
    FROM
      taviperc 
    WHERE
     taviperc.PROCEDUREDATE >= '", fromDate, "' AND
     taviperc.PROCEDUREDATE <= '", toDate, "'", 
    ";")

  rapbase::loadRegData(registryName = registryName,
                       query = query)$AvdRESH
}