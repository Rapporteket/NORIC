#' Kodebok med utledete variabler
#'
#' Legg til utledete variabler i kodebok for NORIC
#' 
#' @return data.frame med kodebok for NORIC med utledete variabler
#' @export
#' @examples 
#'  noric::getKodebokMedUtledetedVar() %>%  
#'  dplyr::filter(fysisk_feltnavn %in% c("uke",
#'                                       "maaned",
#'                                       "indik_trykkmaaling_data",
#'                                       "indik_trykkmaaling"))
getKodebokMedUtledetedVar <- function() {
  noric::def_utledete_var %>% 
    dplyr::select(.data$skjemanavn, 
                  .data$fysisk_feltnavn, 
                  .data$ledetekst, 
                  .data$listeverdier, 
                  .data$listetekst) %>% 
    dplyr::mutate(listeverdier = as.character(.data$listeverdier))
}