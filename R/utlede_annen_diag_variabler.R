#' New variables from NORIC's AnnenDiag-table
#'
#' Functions for creating variables based on NORIC's \emph{AnnenDiag}-table and
#' for merging these variables into NORIC's \emph{AngioPCIVar}-table by key
#' variables \code{ForlopsID} and \code{AvdRESH}.
#'
#' The function \code{utlede_kar_annen_diag()} groups the
#' \code{segments} in \code{df_ad} into coronary arteries in the new variable
#' \code{kar}, then \code{df_ad} is returned from the function.
#'
#' The function \code{legg_til_ffr_per_kar()} uses the data in \code{df_ad} to
#' calculate 5 new variables, \code{FFR_LMS}, \code{FFR_LAD}, \code{FFR_RCA},
#' \code{FFR_CX} and \code{FFR_Graft}, who says whether or not a FFR measure
#' is performed in each \code{kar}. If at least one FFR measure is done in a
#' \code{kar} the value of the corresponding variable is given the value
#' \emph{ja}, otherwise the value is \emph{nei}. These 5 variables are then
#' merged into \code{df_ap}, before \code{df_ap} is returned from the function.
#' For procedures where no measures are done on segment-level (e.g. no entries
#' available in \code{df_ad}), the values for the 5 new variables are <NA>.
#'
#' @param df_ad NORIC's \emph{AnnenDiag}-table, must contain variables
#' \code{ForlopsID}, \code{AVdRESH}, \code{segment}, \code{graft} and
#' \code{metode}
#'
#' @param df_ap NORIC's \emph{AngioPCIVar}-table, must contain variables
#' \code{ForlopsID} and \code{AVdRESH}
#'
#'
#' @name utlede_annenDiag_variabler
#' @aliases utlede_kar_annen_diag legg_til_ffr_per_kar
#'
#' @examples
#' df_ad <- data.frame(
#'    ForlopsID = c(1, 1, 3, 3, 5),
#'    AvdRESH = rep(1,5),
#'    segment = c("PDA/RPD (4)",
#'                "PLA (18)",
#'                "Midtre LAD (7)",
#'                "Ve hovedstamme (5)",
#'                "Ve hovedstamme (5)"),
#'    graft = c("Nei",
#'              "Arteriell",
#'              "Vene",
#'              "Nei",
#'              "Nei"),
#'    metode = c("FFR", "FFR", NA, "test", "FFR"))
#'
#'
#' df_ap <-data.frame(
#'    ForlopsID = c(1, 2, 3, 4, 5),
#'    AvdRESH = rep(1,5))
#'
#'
#' utlede_kar_annen_diag(df_ad)
#' legg_til_ffr_per_kar(df_ap = df_ap,
#'                      df_ad = df_ad)
NULL



#' @rdname utlede_annenDiag_variabler
#' @export
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


#' @rdname utlede_annenDiag_variabler
#' @export
legg_til_ffr_per_kar <- function(df_ap, df_ad) {

  # Must contain matching-variables + variables needed for calculations
  if (!all(c("ForlopsID", "AvdRESH", "segment", "graft", "metode") %in%
           names(df_ad))) {
    stop("df_ad must contain variables ForlopsID, AvdRESH, segment, graft
         and metode")
  }

  # Must contain matching-variables + variables needed for calculations
  if (!all(c("ForlopsID", "AvdRESH") %in%
           names(df_ap))) {
    stop("df_ap must contain variables ForlopsID and AvsRESH")
  }



  # Forberede 5 nye variabler: FFR_LMS, FFR_LAD, FFR_RCA, FFR_CX og FFR_Graft
  ad_wide_ffr <- df_ad %>%

    # Legge til variabel kar_graft
    noric::utlede_kar_annen_diag(.) %>%
    dplyr::select(.data$ForlopsID,
                  .data$AvdRESH,
                  .data$kar,
                  .data$metode) %>%
    dplyr::arrange(.data$AvdRESH, .data$ForlopsID, .data$kar) %>%

    # Teller kun metode = "FFR"
    # Dersom 0 prosedyrer i karet (ingen FFR) blir verdien n=0 --> "nei"
    # Dersom minst en prosedyre (FFR) i karet blir verdien n > 0 --> "ja"
    dplyr::count(.data$AvdRESH, .data$ForlopsID, .data$kar,
                 wt = .data$metode == "FFR") %>%
    dplyr::mutate(ffr_kar = ifelse(
      test = .data$n > 0,
      yes = "ja",
      no = "nei")) %>%
    dplyr::select(- .data$n) %>%
    dplyr::distinct() %>%

    # For alle kombinasjoner av ForlopsID og AvdRESH som har minst en rad i
    # datasettet AP (finner dem med funksjonen nesting),
    # komplettes manglende nivåer av kar med verdien "nei"
    tidyr::complete(.data$kar,
                    tidyr::nesting(ForlopsID, AvdRESH),
                    fill = list(ffr_kar = "nei")) %>%

    # format med en rad per variabel:
    tidyr::pivot_wider(names_from = .data$kar,
                       values_from = .data$ffr_kar) %>%

    # Rekkefølge nye variabler, og nytt navn
    dplyr::select(.data$AvdRESH,
                  .data$ForlopsID,
                  .data$LMS,
                  .data$LAD,
                  .data$RCA,
                  .data$CX,
                  .data$Graft) %>%
    dplyr::rename_at(vars(.data$LMS:.data$Graft),
                     function(x) paste0("FFR_", x))


  # Returnere df_ap, hvor de 5 nye variablene er lagt til.
  # Forløp i AP som ikke har rader i AD, vil få verdien NA for de nye kolonnene.
  dplyr::left_join(df_ap,
                   ad_wide_ffr,
                   by = c("AvdRESH", "ForlopsID"))

}





#' @rdname utlede_annenDiag_variabler
#' @export
legg_til_ifr_per_kar <- function(df_ap, df_ad) {}

#' @rdname utlede_annenDiag_variabler
#' @export
legg_til_ivus_per_kar <- function(df_ap, df_ad) {}

#' @rdname utlede_annenDiag_variabler
#' @export
legg_til_oct_per_kar <- function(df_ap, df_ad) {}

