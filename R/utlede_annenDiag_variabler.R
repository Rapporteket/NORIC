#' Add variable `kar` to NORIC Annen diagnostikk -table
#'
#' @param df_ad annen-diagnostikk table, must contain variables `ForlopsID`,
#' `AVdRESH`, `segment` and `graft`
#'
#' @return annenDiag-table with one new variable
#' @export
#'
#' @examples
#' x <- data.frame(ForlopsID = 1:5,
#'                 AvdRESH = rep(1,5),
#'                 segment = c("PDA/RPD (4)",
#'                             "PLA (18)",
#'                             "Midtre LAD (7)",
#'                             "Ve hovedstamme (5)",
#'                             "Ve hovedstamme (5)"),
#'                 graft = c("Nei", "Arteriell", "Vene", "Nei", "Vene"))
#' x %>% utlede_kar_annen_diag(.)
utlede_kar_annen_diag <- function(df_ad = ad) {


  # Must contain matching-variables + variables needed for calculations
  if (!all(c("ForlopsID", "AvdRESH", "segment", "graft") %in% names(df_ad))) {
    stop("df_ad must contain variables ForlopsID, AVdRESH, segment and graft")
  }

  df_ad %>%
    dplyr::mutate(
      kar = factor(dplyr::case_when(
        .data$graft %in% c("Arterie", "Vene") ~ "Graft",
        .data$segment %in% c("Proximale RCA (1)",
                             "Midtre RCA (2)",
                             "Distale RCA (3)",
                             "PDA/RPD (4)",
                             "PLA (18)",
                             "Høyrekammergren (19)") ~ "RCA",
        .data$segment %in% c("Proximale LAD (6)",
                             "Midtre LAD (7)",
                             "Distale LAD (8)",
                             "Første diagonal (9)",
                             "Andre diagonal (10)",
                             "Septal (20)")   ~ "LAD",
        .data$segment %in% c("Proximale LCx (11)",
                             "Første obtusa marginal (12)",
                             "Andre obtusa marginal (13)",
                             "Distale LCx (14)",
                             "LPD (15)",
                             "PLA fra venstre (16)",
                             "Intermediær (17)")  ~ "CX",
        .data$segment == "Ve hovedstamme (5)" ~ "LMS",
        TRUE ~ NA_character_),

        levels = c("LMS",
                   "LAD",
                   "RCA",
                   "CX",
                   "Graft")))
}


