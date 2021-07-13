#' Add time variables in data frame
#' Year, month, quarter, week..
#'
#' @param df data.frame where variables should be added
#' @param var variable in format "%Y-%m-%d".
#'
#' @return `df` data.frame with new variables added
#' @export
#' \dontrun{
#' # FOR AP, AK and CT-data:
#' AP %<>% legg_til_tidsvariabler(., var = ProsedyreDato)
#' AK %<>% legg_til_tidsvariabler(., var = ProsedyreDato)
#' CT %<>% legg_til_tidsvariabler(., var = HovedDatp)
#' }
legg_til_tidsvariabler <- function(df, var = ProsedyreDato) {

  message("Legger til variablene aar, uke, maaned,
          maaned_nr, kvartal og aar_uke")



  df %>% dplyr::mutate(


    # Kalenderår for ProsedyreDato:
    aar = as.ordered(lubridate::year(as.Date({{ var }},
                                             format = "%Y-%m-%d"))),

    # Måned: månedsnr er tosifret; 01, 02, ....
    maaned_nr = as.ordered(sprintf(
      fmt = "%02d",
      lubridate::month(as.Date({{ var }}, format = "%Y-%m-%d")))),

    maaned = as.ordered(paste0(.data$aar,
                               "-",
                               .data$maaned_nr)),

    # Kvartal:
    kvartal = lubridate::quarter(as.Date({{ var }},
                                         format = "%Y-%m-%d"),
                                 with_year = TRUE),
    kvartal = as.ordered(gsub("[[:punct:]]", "-Q", .data$kvartal)),

     # Uketall:
    uke = as.ordered(sprintf(
      fmt = "%02d",
      lubridate::isoweek(as.Date({{ var }}, format = "%Y-%m-%d")))),

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
      yes = paste0(
        as.integer(lubridate::year(as.Date({{ var }},
                                           format = "%Y-%m-%d"))) - 1,
        "-",
        .data$uke),
      no = .data$aar_uke),

    aar_uke = as.ordered(.data$aar_uke)
  )
}
