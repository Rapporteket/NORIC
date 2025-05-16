#' Erstatt kode med etiketter i data.frame basert på kodebok
#'
#' @param df data.frame, er kun kvalitetssikret til å fungere med angiopcinum
#' @return data.frame med etiketter i stedet for koder
#'
#' @export
#' 
erstatt_koder_m_etiketter <- function(df, mapping = noric::map_num_tekst) {
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
