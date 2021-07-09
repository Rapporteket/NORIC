#' Title Endrer sykehusnavn til ønsket navn
#'
#' @param df data.frame med data, må inneholde kolonnen `AvdRESH`
#' @return data.frame med kolonnen `Sykehusnavn`. Denne vil ha overskrevet den
#' gamle verdien dersom variabelen fanstes i `df` allerede.
#' @export
#'
#' @examples
fikse_sykehusnavn <- function(df) {
  require(dplyr); require(magrittr)

  df %>%
    dplyr::mutate(Sykehusnavn = dplyr::case_when(
      AvdRESH == 108141 ~ "Ahus Nordbyhagen" ,
      AvdRESH == 102966 ~ "HUS",
      AvdRESH == 106944 ~ "AHUS Gardermoen",
      AvdRESH == 4210141 ~ "NLSH Bodø",
      AvdRESH == 700422 ~ "OUS Rikshospitalet",
      AvdRESH == 114150 ~ "SSHF Arendal",
      AvdRESH == 104284 ~ "St.Olavs hospital",
      AvdRESH == 105502 ~ "SUS",
      AvdRESH == 109880 ~ "OUS Ullevål",
      AvdRESH == 101619 ~ "UNN Tromsø",
      TRUE ~ NA_character_)
    )
}

