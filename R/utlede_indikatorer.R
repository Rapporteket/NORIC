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
#'
#'
#' @param df_ap NORIC's \code{AngioPCIVar}-table. Depending on indicators, must
#' contain some of the variables \code{SkjemaStatusKomplikasjoner},
#' \code{Indikasjon}, \code{FFR}, \code{IFR}, ....
#'
#'
#' @name utlede_kvalitesindikatorer
#' @aliases ki_ferdigstilt_komplikasjoner ki_trykkmaaling_utfoert
#'
#' @examples
#'  x <- data.frame(
#'  SkjemaStatusKomplikasjoner = c(-1, 1, 0, NA, NA, NA),
#'  Indikasjon = c(rep("Stabil koronarsykdom", 4), NA, "annet"),
#'  FFR = c("Ja", "Ja", NA, "Ukjent", "Nei", "Ja"),
#'  IFR = c("Ja", "Nei", "Ukjent", NA, NA, NA))
#'
#'  noric::ki_ferdigstilt_komplikasjoner(df_ap = x)
#'  noric::ki_trykkmaaling_utfoert(df_ap = x)
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
