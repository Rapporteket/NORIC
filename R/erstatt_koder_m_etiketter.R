#' Erstatt kode med etiketter i data.frame basert på kodebok
#'
#' @param df data.frame. Denne er kvalitetssikret til å fungere med angiopcinum, 
#' andreprosedyrervarnum, annendiagnostikkvarnum, ctangiovarnum, segmentstentnum,
#' mitralklaffvarnum, aortaklaffvarnum
#' Datarammene som kan legges inn i mapping = noric::
#' angp_map_num_tekst; APVN_map_num_tekst; ADVN_map_num_tekst; CTANG_map_num_tekst;
#' segm_map_num_tekst; mitr_map_num_tekst; aort_map_num_tekst 
#' @return data.frame med etiketter i stedet for koder
#'
#' @export
#' 
erstatt_koder_m_etiketter <- function(df, mapping = noric::angp_map_num_tekst) {
  df_label <- df %>%  
    dplyr::mutate(
      dplyr::across(
        intersect(names(df), mapping$variabel_id), 
        ~ factor(.x, 
                 levels = dplyr::filter(mapping, 
                                        variabel_id %in% 
                                          dplyr::cur_column())$verdi,
                 labels = dplyr::filter(mapping, 
                                        variabel_id %in% 
                                          dplyr::cur_column())$verditekst))
    )
}



