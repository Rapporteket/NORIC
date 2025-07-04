#' Make mapping between unit name and id
#'
#' @param registryName Character string providing registry name key
#' @param asNamedList Logical whether to return a list of named values or not.
#' Default is FALSE in which case a data frame containing name and id is
#' returned.
#' @param newNames Logical whether to provide updated hospital names (TRUE) or 
#' hospital names existing in database (FALSE). Default is FALSE. 
#'
#'@importFrom stats setNames
#'
#' @return A data frame with columns name and id or a list of named ids
#' @export

mapOrgReshId <- function(registryName, asNamedList = FALSE, newNames = FALSE) {

  query <- "
SELECT
  Sykehusnavn AS name,
  AvdResh AS id
FROM
  skjemaoversikt
GROUP BY
  Sykehusnavn,
  AvdResh;"

  res <- rapbase::loadRegData(registryName, query)

  if(newNames){
   res %<>%  
      dplyr::mutate(AvdRESH = id) %>% 
      noric::fikse_sykehusnavn(.) %>% 
      dplyr::select(id, Sykehusnavn) %>% 
      dplyr::rename("name" = "Sykehusnavn")
  }
  
  if (asNamedList) {
    res <- setNames(res$id, res$name)
    res <- as.list(res)
  }

  res
}
