#' NORIC tavi prom 
#' 
#' Functions for adding "listetekst" for variables i \emph{TaviProm}-table. 
#'
#' @param df AortaklaffVar tabell
#'
#' @return df, with new variables suffixed \emph{_tekst}. 
#'
#' @name tavi_funksjoner
#' @aliases 
#' legg_til_taviStatus
#' 
#' @examples
#' data.frame(ePromStatus  = 0:4) %>% noric::legg_til_taviStatus()
#' 
#' @rdname tavi_funksjoner
#' @export
legg_til_taviStatus <- function(df){
  stopifnot("ePromStatus" %in% names(df))
  
  df %>% dplyr::mutate(ePromStatus_tekst= dplyr::case_when(
    ePromStatus %in% 0 ~ "created", 
    ePromStatus %in% 1 ~ "ordered", 
    ePromStatus %in% 2 ~ "expired", 
    ePromStatus %in% 3 ~ "completed", 
    ePromStatus %in% 4 ~ "failed",
    TRUE ~ NA_character_)) %>% 
    dplyr::relocate(ePromStatus_tekst, .after = ePromStatus)
}

legg_til_taviRose <- function(df){
  
  stopifnot(all(c("rose01", 
                  "rose02",
                  "rose03",
                  "rose04",
                  "rose05") %in% names(df)))
  
  df %>% dplyr::mutate(ePromStatus_tekst= dplyr::case_when(
    ePromStatus %in% 0 ~ "created", 
    ePromStatus %in% 1 ~ "ordered", 
    ePromStatus %in% 2 ~ "expired", 
    ePromStatus %in% 3 ~ "completed", 
    ePromStatus %in% 4 ~ "failed",
    TRUE ~ NA_character_)) %>% 
    dplyr::relocate(ePromStatus_tekst, .after = ePromStatus)
}