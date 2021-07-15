#' Add time-variables
#'
#' The function \code{legg_til_tidsvariabler()} creates six time-variables, \code{aar},
#' \code{maaned_nr}, \code{maaned}, \code{kvartal}, \code{uke}, \code{aar_uke},
#' based on a given date-column (\code{var}) of input data.frame \code{df}. Then these new variables
#' are added to \code{df} before the entire data.frame is returned.
#'
#' @param df data.frame from any NORIC-table. Must contain a variable
#' in date-format.
#' @param var name of variable in date-format that should be used to
#' calculate the new variables.
#'
#' @return The input data.frame \code{df} is returned as it is, with six new columns,
#' one for each time-variable.
#'
#' @examples
#' df <- data.frame(
#'    datoVariabel = c("2021-02-28",
#'                     "2013-09-15",
#'                     "2020-05-31",
#'                     "2015-12-12",
#'                     "2016-12-31",
#'                     "2016-01-01" ))
#' legg_til_tidsvariabler(df = df,
#'                        var = datoVariabel)
#'
#' \dontrun{
#' # FOR NORIC's ap, ak and ct-tables:
#' legg_til_tidsvariabler(ap, var = ProsedyreDato)
#' legg_til_tidsvariabler(ak, var = ProsedyreDato)
#' legg_til_tidsvariabler(ct, var = UndersokDato)
#' }
#' @export
legg_til_tidsvariabler <- function(df, var = ProsedyreDato) {


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
