#' Legg tid tidsvariabler
#' Aar, maaned_nr, maaned, kvartal, uketall, aar_uke
#'
#' @param df data.frame der variablene skal legges til, må inneholde en
#' dato-variabel i format "%Y-%m-%d"
#' @param var for AngioPCI tabellen brukes `ProsedyreDato`, for CT brukes `HovedDato`
#'
#' @return `df` med de nye variablene
#' @export
#'
#' @examples
legg_til_tidsvariabler <- function(df, var = ProsedyreDato){
  require(tidyverse)

  message("Legger til variablene aar, uke, maaned, maaned_nr, kvartal og aar_uke")



  df %>% dplyr::mutate(


    # Kalenderår for ProsedyreDato:
    aar = as.ordered(lubridate::year(as.Date({{ var }},
                                             format = "%Y-%m-%d"))),

    # Måned: månedsnr er tosifret; 01, 02, ....
    maaned_nr = as.ordered(sprintf(fmt = "%02d",
                                   lubridate::month(as.Date({{ var }},
                                                            format = "%Y-%m-%d")))),
    maaned = as.ordered(paste0(.data$aar,
                               "-",
                               .data$maaned_nr)),

    # Kvartal:
    kvartal = lubridate::quarter(as.Date({{ var }},
                                         format = "%Y-%m-%d"),
                                 with_year = TRUE),
    kvartal = as.ordered(gsub("[[:punct:]]", "-Q", .data$kvartal)),

     # Uketall:
    uke = as.ordered(sprintf(fmt = "%02d",
                             lubridate::isoweek(as.Date({{ var }},
                                                        format = "%Y-%m-%d")))),

    # Variabel med "yyyy-ukenummer" som tar høyde for uketall spredt over to
    # kalenderår:

    aar_uke = ifelse(
      # hvis uke 01 er i desember...
      test = .data$uke == "01" & .data$maaned_nr == "12",

       # .. så sier vi at uken tilhører det seneste av de to årene som uke 01
      # er spredt over (uke 01 i desember 2019 blir til 2020-01)
      yes = paste0(as.integer(lubridate::year({{ var }})) + 1, "-",
                   .data$uke),
      no = paste0(.data$aar, "-", .data$uke)),

    aar_uke = ifelse(
      # hvis uke 52 eller 53 er i januar...
      test = .data$uke %in% c("52", "53") & .data$maaned_nr == "01",
      # ...sier vi at hele uken tilhører det tidligste av de to årene som uke
      # 52/53 er spredt over (1. januar 2017 som er i uke 52 blir til 2016-52)
      yes = paste0(as.integer(lubridate::year(as.Date({{ var }},
                                                      format = "%Y-%m-%d"))) - 1,
                   "-",
                   .data$uke),
      no = .data$aar_uke),

    aar_uke = as.ordered(.data$aar_uke)
  )
}
