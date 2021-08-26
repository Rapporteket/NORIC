#' NORIC's quality indicators
#'
#' Functions for creating two new variables for each indicator:
#' The first variable says whether the procedure counts (ja/nei) in the
#' denominator for the given indicator and the second says whether
#' the procedure counts in the numerator (ja/nei/NA). If the denominator has
#' value \emph{nei} the numerator should always be \emph{NA}. Denominator
#' variables names are suffixed by \emph{_dg} (datagrunnlag).
#'
#' Following variables are created for the different functions:
#'
#' \code{ki_ferdigstilt_komplikasjoner()}
#'  \itemize{
#'  \item denominator \code{ki_komplik_ferdig_dg} (datagrunnlag) is
#'  \emph{ja} when \emph{Regtype = Primaer}.
#'  \item numerator \code{ki_komplik_ferdig} has value \emph{ja} if the
#'  complications form is completed (komplikasjons-skjema er ferdigstilt).}
#'
#'
#'
#' \code{ki_trykkmaaling_utfoert()}
#' \itemize{
#' \item denominator \code{ki_trykkmaaling_dg} (datagrunnlag) is \emph{ja} when
#'  \emph{Indikasjon = Stabil koronarsykdom}.
#' \item numerator \code{ki_trykkmaaling} has value \emph{ja} if FFR and/or iFR
#' has been performed.}
#'
#' \code{ki_ivus_oct_ved_stenting_lms()}
#' \itemize{
#' \item denominator \code{ki_ivus_oct_ved_stenting_lms_dg} (datagrunnlag) is
#' \emph{ja} when all of these conditions are fulfilled:
#' \enumerate{
#'   \item \code{Indikasjon} is one of "Vitieutredning", "Uklare brystsmerter",
#'  "Annet", "Hjertestans uten STEMI", "Hjertesvikt/kardiomyopati",
#'   "Komplettering av tidligere PCI", "UAP", "NSTEMI",
#'   "Stabil koronarsykdom"
#'   \item \code{satt_inn_stent_i_LMS} = "ja"
#'   \item \code{TidlABC} is one of "Nei", "Ukjent", NA
#'   }
#' \item numerator \code{ki_ivus_oct_ved_stenting_lms} has value \emph{ja} if
#' IVUS and/or OCT has been performed.
#' }
#'
#'
#' @param df_ap NORIC's \code{AngioPCIVar}-table. Depending on indicators, must
#' contain some of the variables \code{SkjemaStatusKomplikasjoner},
#' \code{Indikasjon}, \code{FFR}, \code{IFR}, ....
#'
#'
#' @name utlede_kvalitesindikatorer
#' @aliases
#' ki_ferdigstilt_komplikasjoner
#' ki_trykkmaaling_utfoert
#' ki_ivus_oct_ved_stenting_lms
#'
#' @examples
#'  x <- data.frame(
#'  SkjemaStatusKomplikasjoner = c(-1, 1, 0, NA, NA, NA))
#'  noric::ki_ferdigstilt_komplikasjoner(df_ap = x)
#'
#'  x <- data.frame(
#'  Indikasjon = c(rep("Stabil koronarsykdom", 4), NA, "annet"),
#'  FFR = c("Ja", "Ja", NA, "Ukjent", "Nei", "Ja"),
#'  IFR = c("Ja", "Nei", "Ukjent", NA, NA, NA))
#'  noric::ki_trykkmaaling_utfoert(df_ap = x)
#'
#'  x <- data.frame(
#'  Indikasjon = c(rep("Stabil koronarsykdom", 4), NA, "Annet"),
#'  TidlABC = rep("Nei", 6),
#'  satt_inn_stent_i_LMS = c(rep("ja", 4), NA, "nei"),
#'  IVUS = c("Ja", "Ja", NA, "Ukjent", "Nei", "Ja"),
#'  OCT = c("Ja", "Nei", "Ukjent", NA, NA, NA))
#'  noric::ki_ivus_oct_ved_stenting_lms(df_ap = x)

NULL

#' @rdname utlede_kvalitesindikatorer
#' @export
ki_ferdigstilt_komplikasjoner <- function(df_ap) {

  stopifnot("SkjemaStatusKomplikasjoner" %in% names(df_ap))


  df_ap %>%
    dplyr::mutate(

      # Datagrunnlag for indikatoren
      ki_komplik_ferdig_dg = dplyr::if_else(
        condition = .data$SkjemaStatusKomplikasjoner %in% c(-1, 0, 1),
        true = "ja",
        false = "nei",
        missing = "nei"),

      # utlede verdi for indikatoren dersom datagrunnlag = "ja"
      ki_komplik_ferdig = dplyr::case_when(

        .data$ki_komplik_ferdig_dg == "ja" &
          .data$SkjemaStatusKomplikasjoner == 1 ~ "ja",

        .data$ki_komplik_ferdig_dg == "ja" &
          .data$SkjemaStatusKomplikasjoner %in% c(-1, 0) ~ "nei",

        .data$ki_komplik_ferdig_dg == "nei" ~ NA_character_,

        FALSE ~ NA_character_))
}


#' @rdname utlede_kvalitesindikatorer
#' @export
ki_trykkmaaling_utfoert <- function(df_ap) {
  stopifnot(all(c("Indikasjon", "FFR", "IFR") %in% names(df_ap)))


  df_ap %>%
    dplyr::mutate(

      # Datagrunnlag for indikatoren
      ki_trykkmaaling_dg = dplyr::if_else(
        condition = .data$Indikasjon %in% c("Stabil koronarsykdom"),
        true = "ja",
        false = "nei",
        missing = "nei"),

      # utlede verdi for indikatoren dersom datagrunnlag = "ja"
      # IFR og/eller FFR er utført
      ki_trykkmaaling = dplyr::case_when(

        .data$ki_trykkmaaling_dg == "ja" &
          (.data$FFR == "Ja" | .data$IFR == "Ja") ~ "ja",

        .data$ki_trykkmaaling_dg == "ja" &
          (.data$FFR != "Ja" & .data$IFR != "Ja") ~ "nei",

        .data$ki_trykkmaaling_dg == "ja" &
          is.na(.data$FFR) & .data$IFR != "Ja" ~ "nei",

        .data$ki_trykkmaaling_dg == "ja" &
          .data$FFR != "Ja" & is.na(.data$IFR) ~ "nei",

        .data$ki_trykkmaaling_dg == "ja" &
          is.na(.data$FFR) & is.na(.data$IFR) ~ "nei",

        .data$ki_trykkmaaling_dg == "nei" ~ NA_character_,

        FALSE ~ NA_character_))
}






#' @rdname utlede_kvalitesindikatorer
#' @export
ki_ivus_oct_ved_stenting_lms <- function(df_ap) {
  stopifnot(all(c("Indikasjon","TidlABC", "IVUS",
                  "OCT", "satt_inn_stent_i_LMS")
                %in% names(df_ap)))


  df_ap %>%
    dplyr::mutate(

      # Datagrunnlag for indikatoren
      #  ~ IKKE tidligere ACB-operert
      #  ~ en av indikasjonene i listen
      #  ~ prosedyre med stenting av venstre hovedstamme (Segment5 = LMS = VH)
      ki_ivus_oct_ved_stenting_lms_dg = dplyr::if_else(
        condition =
          (.data$TidlABC %in% c("Nei", "Ukjent") | is.na(.data$TidlABC)) &
          .data$Indikasjon %in% c("Vitieutredning",
                                  "Uklare brystsmerter",
                                  "Annet",
                                  "Hjertestans uten STEMI",
                                  "Hjertesvikt/kardiomyopati",
                                  "Komplettering av tidligere PCI",
                                  "UAP",
                                  "NSTEMI",
                                  "Stabil koronarsykdom") &
          .data$satt_inn_stent_i_LMS == "ja",

        true = "ja",
        false = "nei",
        missing = "nei"),

      # utlede verdi for indikatoren dersom datagrunnlag = "ja"
      # IVUS og/eller OCT er utført
      ki_ivus_oct_ved_stenting_lms = dplyr::case_when(

        .data$ki_ivus_oct_ved_stenting_lms_dg == "ja" &
          (.data$IVUS == "Ja" | .data$OCT == "Ja") ~ "ja",

        .data$ki_ivus_oct_ved_stenting_lms_dg == "ja" &
          (.data$IVUS != "Ja" & .data$OCT != "Ja") ~ "nei",

        .data$ki_ivus_oct_ved_stenting_lms_dg == "ja" &
          is.na(.data$IVUS) & .data$OCT != "Ja" ~ "nei",

        .data$ki_ivus_oct_ved_stenting_lms_dg == "ja" &
          .data$IVUS != "Ja" & is.na(.data$OCT) ~ "nei",

        .data$ki_ivus_oct_ved_stenting_lms_dg == "ja" &
          is.na(.data$IVUS) & is.na(.data$OCT) ~ "nei",

        .data$ki_ivus_oct_ved_stenting_lms_dg == "nei" ~ NA_character_,

        FALSE ~ NA_character_))
}
