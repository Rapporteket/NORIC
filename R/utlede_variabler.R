


#' Add variable `aldersklasse` - decades
#'
#' Under 18 years og above 99 are set as NA
#' @param df data.frame where the new variable should be added.
#' @param var Variable name of the numerical variable that contains age as
#' a continuous value. Default variable name is `PasientAlder`
#' @return data.frame with new variable
#'
#' @export
#' @examples
#' df <-  data.frame(age = c(10,20,59,60, NA, 61,69,70,99,100))
#' df %>% utlede_aldersklasse(., var = age)
utlede_aldersklasse <- function(df, var = PasientAlder) {
  df %>%
    dplyr::mutate(
      aldersklasse = cut(
        replace_na({{var}}, replace = 0),
        breaks = c(18, 49, 59, 69, 79, 89, 99),
        include.lowest = TRUE,
        labels = c("18-49", "50-59", "60-69", "70-79", "80-89", "90-99"),
        ordered_result = TRUE))

}



#' Add binary variable ("ja", "nei") for table status
#'
#' @param df data.frame where variable should be added
#' @param var variable name of the numerical staus-variable with
#' values {-1,0,1}
#' @param suffix wanted suffix in the added variable's name:ferdigstilt_`suffix`
#' @return data.frame with new variable
#'
#' @export
#' @examples
#' x <- data.frame(SkjemaStatusStart = c(-1,-1,0,0,1,-1,0,1,NA),
#'                 SkjemastatusHovedskjema = rep(0,9),
#'                 SkjemaStatusUtskrivelse = rep(-1,9),
#'                 SkjemaStatusKomplikasjoner = c(rep(-1,4), rep(1,3), NA, NA))
#' x %>% utlede_ferdigstilt(., var = SkjemaStatusStart, suffix = "startSkjema")
#' x %>%
#' utlede_ferdigstilt(., var = SkjemaStatusStart, suffix = "startSkjema") %>%
#' utlede_ferdigstilt(., var = SkjemastatusHovedskjema,
#' suffix = "hovedSkjema") %>%
#' utlede_ferdigstilt(., var = SkjemaStatusKomplikasjoner,
#'  suffix = "komplikSkjema")

utlede_ferdigstilt <- function(df,
                               var = SkjemaStatusStart,
                               suffix = "startSkjema") {

  df %>%  dplyr::mutate(
    "ferdigstilt_{suffix}" := dplyr::if_else({{ var }} == 1,
                                             true = "ja",
                                             false = "nei",
                                             missing = NA_character_))
  }
