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
  
  
  
  
  noric::getKodebokData() %>% 
    dplyr::select(.data$skjemanavn, 
                  .data$fysisk_feltnavn, 
                  .data$ledetekst, 
                  .data$listeverdier, 
                  .data$listetekst, 
                  .data$aktiverinsspoersmaal, 
                  .data$underspoersmaal, 
                  .data$innfoert_dato, 
                  .data$tabell) %>% 
    dplyr::mutate(listeverdier = as.character(.data$listeverdier)) %>%
    dplyr::bind_rows(noric::def_utledete_var %>%
                       dplyr::select(.data$skjemanavn,
                                     .data$fysisk_feltnavn,
                                     .data$ledetekst, 
                                     .data$listeverdier, 
                                     .data$listetekst) %>% 
                       tidyr::replace_na(replace = list(listeverdier= "NA")))
  
  

}



#' Kodebok
#'
#'Hent kodebok (metadata) for alle NORIC variabler. Merk: Kun tavi prom i 
#'første versjon.
#' @return kodebok for taviprom (føsrte versjon)
#' @export
#'
#' @examples
#' noric::getKodebokData() %>% 
#' select(fysisk_feltnavn == "heart01")
getKodebokData <- function() {
  noric::kb
}