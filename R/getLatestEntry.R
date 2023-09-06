#' Get the latest registration date from the registry database
#'
#' Provides the newest date based on HovedDato from SkjemaOversikt
#'
#' @param registryName Character string providing registry name key
#' @param reshID Numerical for local registry
#' @return A Date object

#' @name getLatestEntry
#' @aliases getLatestEntryHospital
#' NULL
#' 
#' @rdname getLatestEntry
#' @export
getLatestEntry <- function(registryName) {

  query <- paste0("
SELECT
  max(HovedDato) AS date
FROM
  SkjemaOversikt;"
                  )

  rapbase::loadRegData(registryName, query = query)$date
}

#' @rdname getLatestEntry
#' @export
getLatestEntryHospital <- function(registryName, reshID = 99999) {
  
  query <- paste0("
SELECT
  max(HovedDato) AS date
FROM
  SkjemaOversikt 
WHERE AvdRESH=", 
  reshID, ";"  )
  
  rapbase::loadRegData(registryName, query = query)$date
}