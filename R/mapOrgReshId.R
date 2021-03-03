#' Make mapping between unit name and id
#'
#' @param registryName Character string providing registry name key
#'
#' @return A data frame with columns name and id
#' @export

mapOrgReshId <- function(registryName) {
  
  query <- "
SELECT
  Sykehusnavn AS name,
  AvdResh AS id
FROM
  SkjemaOversikt
GROUP BY
  Sykehusnavn,
  AvdResh;"
  
  rapbase::loadRegData(registryName, query)
}