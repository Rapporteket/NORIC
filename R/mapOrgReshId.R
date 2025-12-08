#' Make mapping between unit name and id
#'
#' @param registryName Character string providing registry name key
#' @param asNamedList Logical whether to return a list of named values or not.
#' Default is FALSE in which case a data frame containing name and id is
#' returned.
#'
#'@importFrom stats setNames
#'
#' @return A data frame with columns name and id or a list of named ids
#' @export

mapOrgReshId <- function(registryName, asNamedList = FALSE) {
  
  query <- "SELECT DISTINCT mce.CENTREID AS id FROM mce ;"
  res <- rapbase::loadRegData(registryName, query)
  
  res %<>%
    dplyr::mutate(AvdRESH = id) %>% 
    noric::fikse_sykehusnavn(.) %>% 
    dplyr::select(id, Sykehusnavn) %>% 
    dplyr::rename("name" = "Sykehusnavn") %>% 
    dplyr::arrange(name)
  
  if (asNamedList) {
    res <- setNames(res$id, res$name)
    res <- as.list(res)
  }
  
  res
}
