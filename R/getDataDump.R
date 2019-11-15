#' Query data for dumps
#'
#' @param registryName String registry name
#' @param tableName String with name of table to query from
#' @param fromDate String with start period, endpoint included
#' @param toDate String with end period, endpont included
#'
#' @return A data frame with registry data
#' @export
#'

getDataDump <- function(registryName, tableName, fromDate, toDate) {
  
  query <- paste0("
SELECT
  p.*,
  so.HovedDato
FROM
  ", tableName, " p
LEFT JOIN
  SkjemaOversikt so
ON
  p.ForlopsID = so.ForlopsID AND p.AvdRESH = so.AvdRESH
WHERE
  so.HovedDato >= '", fromDate, "' AND so.HovedDato <= '", toDate, "';"
                  )
  
  rapbase::LoadRegData(registryName, query)
}
