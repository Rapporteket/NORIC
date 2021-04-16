#' Get the latest registration date from the registry database
#' 
#' Provides the newest date based on HovedDato from SkjemaOversikt
#'
#' @param registryName 
#'
#' @return A Date object
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