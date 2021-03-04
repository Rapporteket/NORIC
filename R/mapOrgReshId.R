#' Make mapping between unit name and id
#'
#' @param registryName Character string providing registry name key
#'
#' @return A data frame with columns name and id
#' @export

mapOrgReshId <- function(registryName, asNamedList = FALSE) {

  query <- "
SELECT
  Sykehusnavn AS name,
  AvdResh AS id
FROM
  SkjemaOversikt
GROUP BY
  Sykehusnavn,
  AvdResh;"

  res <- rapbase::loadRegData(registryName, query)

  if (asNamedList) {
    res <- setNames(res$id, res$name)
    res <- as.list(res)
  }

  res
}