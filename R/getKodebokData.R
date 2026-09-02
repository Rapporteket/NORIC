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
    dplyr::select(skjemanavn, 
                  fysisk_feltnavn, 
                  ledetekst, 
                  listeverdier, 
                  listetekst, 
                  aktiveringsspm, 
                  underspm, 
                  innfort, 
                  tabell) %>% 
    dplyr::mutate(listeverdier = as.character(listeverdier)) %>%
    dplyr::bind_rows(noric::def_utledete_var %>%
                       dplyr::select(skjemanavn,
                                     fysisk_feltnavn,
                                     ledetekst, 
                                     listeverdier, 
                                     listetekst) %>% 
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
#'   dplyr::filter(fysisk_feltnavn == "heart01")
getKodebokData <- function() {
  noric::kb %>% 
    dplyr::rename("aktiveringsspm" = aktiverinsspoersmaal, 
                  "underspm" = underspoersmaal, 
                  "innfort" = innfoert_dato)
}