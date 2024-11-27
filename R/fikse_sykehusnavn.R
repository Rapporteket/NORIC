#' Add variable `Sykehusnavn` if missing, update values if existing
#'
#' @param df data.frame, must contain variable `AvdRESH`
#' @return data.frame with variable `Sykehusnavn`. Old values are overwritten
#' if `Sykehusnavn` already existed if `df`
#'
#' @export
#' @examples
#' x <- data.frame(AvdRESH = c(108141, 109880, NA, 123, 105502))
#' x %>% fikse_sykehusnavn()
#'
#' y <- data.frame(AvdRESH = c(108141, 109880, NA, 123, 105502),
#'                 Sykehusnavn = c("AHUS", "Ullevål", "NA",
#'                  "test", "Stavanger"))
fikse_sykehusnavn <- function(df) {

  if (!("AvdRESH" %in% names(df))) stop("df must contain variable AvdRESH")

  df %>%
    dplyr::mutate(Sykehusnavn = dplyr::case_when(
      AvdRESH == 108141 ~ "Ahus Nordbyhagen",
      AvdRESH == 102966 ~ "HUS",
      AvdRESH == 106944 ~ "Ahus Gardermoen",
      AvdRESH == 4210141 ~ "UNN Bodø",
      AvdRESH == 700422 ~ "OUS Rikshospitalet",
      AvdRESH == 114150 ~ "SSHF Arendal",
      AvdRESH == 104284 ~ "St.Olavs hospital",
      AvdRESH == 105502 ~ "SUS",
      AvdRESH == 109880 ~ "OUS Ullevål",
      AvdRESH == 101619 ~ "UNN Tromsø",
      AvdRESH == 0 ~ "Nasjonal",
      TRUE ~ NA_character_)) %>%
    dplyr::relocate(.data$Sykehusnavn, .after = .data$AvdRESH)
}
