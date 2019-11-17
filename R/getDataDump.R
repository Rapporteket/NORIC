#' Query data for dumps
#'
#' @param registryName String registry name
#' @param tableName String with name of table to query from
#' @param fromDate String with start period, endpoint included
#' @param toDate String with end period, endpont included
#' @param ... Additional parmeters to be passed to the function
#'
#' @return A data frame with registry data
#' @export
#'

getDataDump <- function(registryName, tableName, fromDate, toDate, ...) {
  
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
  
  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]],
                      msg = paste("NORIC data dump:\n", query))
  }
  
  rapbase::LoadRegData(registryName, query)
}
