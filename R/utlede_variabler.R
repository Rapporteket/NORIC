


#' Utlede aldersklasse -tiår
#' PasientAlder under 18 år eller over 99 år settes som NA.
#'
#' @param df data.frame der variablene skal legges til, må inneholde en
#' numerisk variabel som inneholder pasientens alder.
#' @param var default verdi er `PasientAlder`
#'
#' @return data.frame med en ny variabel
#' @export
#'
#' @examples
utlede_aldersklasse <- function(df, var = PasientAlder){
  df %>%
    mutate(
      aldersklasse = cut(
        replace_na({{var}}, replace = 0),
        breaks = c(18, 49, 59, 69, 79, 89, 99),
        include.lowest = TRUE,
        labels = c("18-49","50-59","60-69","70-79","80-89","90-99"),
        ordered_result = TRUE))

}

