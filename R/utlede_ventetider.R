
#' NSTEMI ventetid
#'
#' Antall timer mellom Ankomst PCI og Prosedyre for direkte innlagte pasienter.
#' Antall timer mellom Innleggelse ved henvisende sykeshus og Prosedyre for
#' overfoerte pasienter. Dersom en av datoene/tidspunktene mangler kan ikke
#' antall timer regnes ut. Dersom  \code{OverflyttetFra} mangler eller er
#' lik "Annen avd paa sykehuset" regnes ikke ventetid ut. Ingen filter paa
#' unormalt lange eller negative, ventetider.
#'
#' @param df_ap tabellen med AngioPCI data fra NORIC. Maa inneholde variablene
#' \code{OverflyttetFra, ProsedyreDato, ProsedyreTid,AnkomstPCIDato,
#'  AnkomstPCITid, InnleggelseHenvisendeSykehusDato,
#'  InnleggelseHenvisendeSykehusTid}
#'
#' @return returnerer \code{df_ap} med en ny kolonne:
#'  \code{ventetid_nstemi_timer}
#' @export
#'
#' @examples
#'x <- data.frame(
#'  OverflyttetFra = c("Omdirigert ambulanse",
#'                     "Nei, direkte inn til dette sykehuset",
#'                     "Annet sykehus", NA),
#'  ProsedyreDato = as.Date(c("2020-01-30", "2021-11-15",
#'                            "2020-11-11", "2021-12-24"),
#'                          format = "%Y-%m-%d"),
#'
#'  ProsedyreTid = c("22:30:00", "01:10:00", "13:45:00", "12:20:00"),
#'    AnkomstPCIDato = as.Date(
#'    c("2020-01-28", "2021-11-14", "2018-04-24", "2020-01-01"),
#'    format = "%Y-%m-%d"),
#'  AnkomstPCITid = c(
#'    "21:10:00", "23:10:00", "05:20:00", NA_character_),
#'  InnleggelseHenvisendeSykehusDato = as.Date(
#'    c(NA, NA,  "2020-11-10", "2021-12-21"),
#'    format = "%Y-%m-%d"),
#'  InnleggelseHenvisendeSykehusTid = c(NA_character_, NA_character_,
#'                                      "11:25:00", "23:20:00"))
#'noric::legg_til_ventetid_nstemi_timer(x)
#'
#'
#'
#'
#'
legg_til_ventetid_nstemi_timer <- function(df_ap){

  stopifnot(all(c("OverflyttetFra",
                  "ProsedyreDato",
                  "ProsedyreTid",
                  "AnkomstPCIDato",
                  "AnkomstPCITid",
                  "InnleggelseHenvisendeSykehusDato",
                  "InnleggelseHenvisendeSykehusTid") %in% names(df_ap)))

  df_ap %>%
    mutate(

      # Midlertidige variabler
      ProsedyreTidspunkt  = lubridate::parse_date_time2(
        paste(.data$ProsedyreDato, .data$ProsedyreTid),
        "%Y-%m-%d %H:%M:%S"),

      AnkomstTidspunkt  = lubridate::parse_date_time2(
        paste(.data$AnkomstPCIDato, .data$AnkomstPCITid),
        "%Y-%m-%d %H:%M:%S"),

      HenvisendeSykehusTidspunkt = lubridate::parse_date_time2(
        paste(.data$InnleggelseHenvisendeSykehusDato,
              .data$InnleggelseHenvisendeSykehusTid),
        "%Y-%m-%d %H:%M:%S"),


      ventetid_nstemi_timer = dplyr::case_when(

        # Hvis direkte innleggelse.
        .data$OverflyttetFra %in%
          c("Nei, direkte inn til dette sykehuset",
            "Omdirigert ambulanse") ~
          round(as.numeric(difftime(.data$ProsedyreTidspunkt ,
                                    .data$AnkomstTidspunkt ,
                                    units = "hours")), 1),

        # Hvis overflyttede pasienter
        .data$OverflyttetFra %in% c("Annet sykehus") ~
          round(as.numeric(difftime(.data$ProsedyreTidspunkt ,
                                    .data$HenvisendeSykehusTidspunkt,
                                    units = "hours")), 1),

     # Manglende eller "Annen avd på sykehuset"
     TRUE ~ NA_real_)) %>%


    # Fjerne midlertidige variabler
    dplyr::select(-.data$ProsedyreTidspunkt,
                  -.data$AnkomstTidspunkt,
                  -.data$HenvisendeSykehusTidspunkt)

}







#' STEMI ventetid
#'
#' Antall minutter fra beslutningsutløsende EKG til arteriepunksjon.
#' Dersom en av datoene/tidspunktene mangler kan ikke ventetiden regnes ut.
#' Ingen filter paa unormalt lange eller negative, ventetider. Dette er en
#' hjelpefunksjon til \link{\code{ki_stemi_utredet_innen12min()}}, ventetidene
#' må ikke analyseres utenfor denne funksjonen da datagrunnlaget defineres der.
#'
#' @param df_ap tabellen med AngioPCI data fra NORIC. Maa inneholde variablene
#' \code{ProsedyreDato, ProsedyreTid,BesUtlEKGDato, BesUtlEKGTid}
#'
#' @return returnerer \code{df_ap} med en ny kolonne:
#'  \code{ventetid_stemi_minutter}
#' @export
#'
#' @examples
#'x <- data.frame(
#'  ProsedyreDato = as.Date(c("2020-01-30", "2021-11-15",
#'                            "2020-11-11", "2021-12-24"),
#'                          format = "%Y-%m-%d"),
#'  ProsedyreTid = c("22:30:00", "01:10:00", "13:45:00", "12:20:00"),
#'  BesUtlEKGDato = as.Date(c("2020-01-30", "2021-11-14",
#'                            "2018-04-24", "2020-01-01"),
#'                          format = "%Y-%m-%d"),
#'  BesUtlEKGTid = c( "21:10:00", "23:10:00", "05:20:00", NA_character_))
#' noric::legg_til_ventetid_stemi_min(x)

legg_til_ventetid_stemi_min <- function(df_ap){

  stopifnot(all(c("ProsedyreDato",
                  "ProsedyreTid",
                  "BesUtlEKGDato",
                  "BesUtlEKGTid",
                  "BeslutningsutlosendeEKG") %in% names(df_ap)))

  df_ap %>%
    mutate(

      # Midlertidige variabler
      ProsedyreTidspunkt  = lubridate::parse_date_time2(
        paste(.data$ProsedyreDato, .data$ProsedyreTid),
        "%Y-%m-%d %H:%M:%S"),

      BesUtlEkgTidspunkt  = lubridate::parse_date_time2(
        paste(.data$BesUtlEKGDato, .data$BesUtlEKGTid),
        "%Y-%m-%d %H:%M:%S"),


      ventetid_stemi_min =
          round(as.numeric(difftime(.data$ProsedyreTidspunkt ,
                                    .data$BesUtlEkgTidspunkt ,
                                    units = "mins")), 1),

      # Fjerner de som har 0 minutters ventetid samtidig som
      # Beslutingsutløsende EKG er Prehospitalt
      ventetid_stemi_min = ifelse(
        .data$ventetid_stemi_min == 0 &
          .data$BeslutningsutlosendeEKG %in% "Prehospitalt",
        yes = NA,
        no = ventetid_stemi_min )) %>%



    # Fjerne midlertidige variabler
    dplyr::select(-.data$ProsedyreTidspunkt,
                  -.data$BesUtlEkgTidspunkt)
}
