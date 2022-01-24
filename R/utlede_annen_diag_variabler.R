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
#' The function \code{legg_til_trykk_bilde_per_kar()} uses the data in
#' \code{df_ad} to calculate 5 new variables, where variable names prefix is
#' type of method (one of: "FFR", "iFR", "IVUS" or "OCT") and suffix is each
#' level of \code{kar} (p.ex. if metode is 'FFR' then variables are
#'  \code{FFR_LAD}, \code{FFR_RCA}, \code{FFR_CX} and \code{FFR_Graft}).
#' The variable says whether or not a measure of this method
#' is performed in each \code{kar}. If at least one measure is done in a
#' \code{kar} the value of the corresponding variable is given the value
#' \emph{ja}, otherwise the value is \emph{nei}. These 5 variables are then
#' merged into \code{df_ap}, before \code{df_ap} is returned from the function.
#' For procedures where no measures are done on segment-level (e.g. no entries
#' available in \code{df_ad}), the values for the 5 new variables are <NA>.
#'
#' The function \code{legg_til_trykkmaalinger()} adds variables \code{PdPa},
#' \code{IMR}, \code{Pa} and \code{Pd} to \code{df_ap}-table. Values of these 4
#' new variables (measures of pressure) depends on the \code{df_ad}-table:
#' value "Ja" is attributed if the method is registered for the procedure,
#' value "Nei" is attributed if no if this method not is performed.
#' For procedures where no measures are done on segment-level (e.g. no entries
#' available in \code{df_ad}), the values for the 4 new variables are <NA>.
#'
#' @param df_ad NORIC's \emph{AnnenDiag}-table, must contain variables
#' \code{ForlopsID}, \code{AVdRESH}, \code{segment}, \code{graft} and
#' \code{metode}
#'
#' @param df_ap NORIC's \emph{AngioPCIVar}-table, must contain variables
#' \code{ForlopsID} and \code{AVdRESH}
#'
#' @param metodeType value of variable \code{metode} from NORIC-table
#' \emph{AnnenDiag}. Must have one of following values : "FFR", "iFR", "IVUS"
#' or "OCT"
#'
#'
#' @name utlede_annenDiag_variabler
#' @aliases
#' utlede_kar_annen_diag
#' legg_til_trykk_bilde_per_kar
#' legg_til_trykkmaalinger
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
#' legg_til_trykk_bilde_per_kar(df_ap = df_ap,
#'                              df_ad = df_ad,
#'                              metodeType = "FFR")
#'
#' df_ad <- data.frame(
#'     ForlopsID = c(1, 1, 3, 3, 5),
#'     AvdRESH = rep(1,5),
#'     metode = c("FFR", "FFR", NA, "test", "Pd/Pa"))
#' noric::legg_til_trykkmaalinger(df_ap = df_ap,
#'                                df_ad = df_ad)
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
legg_til_trykk_bilde_per_kar <- function(df_ap,
                                         df_ad,
                                         metodeType = "FFR") {

  . <- ""
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


  if (!metodeType %in% c("FFR", "iFR", "IVUS", "OCT")) {
    stop("MetodeType er en tekstreng med en verdiene : 'FFR', 'iFR', 'IVUS'
         eller 'OCT'.")
  }

  ad_wide <- df_ad %>%

    # Legge til variabel kar_graft
    utlede_kar_annen_diag(.) %>%
    dplyr::select(.data$ForlopsID,
                  .data$AvdRESH,
                  .data$kar,
                  .data$metode) %>%
    dplyr::arrange(.data$AvdRESH, .data$ForlopsID, .data$kar) %>%

    # Teller kun metode = metodeType
    # Dersom 0 undersøkelser i karet av denne metoden blir verdien n=0-->"nei"
    # Dersom minst en undersøkelse med denne metoden i karet blir verdien
    #    n > 0 --> "ja"
    dplyr::count(.data$AvdRESH, .data$ForlopsID, .data$kar,
                 wt = .data$metode == metodeType) %>%
    dplyr::mutate(
      verdi_kar = ifelse(
        test = .data$n > 0,
        yes = "ja",
        no = "nei")) %>%
    dplyr::select(- .data$n) %>%
    dplyr::distinct() %>%

    # For alle kombinasjoner av ForlopsID og AvdRESH som har minst en rad i
    # datasettet AD (finner dem med funksjonen nesting),
    # komplettes manglende nivåer av kar med verdien "nei"
    tidyr::complete(.data$kar,
                    tidyr::nesting(ForlopsID, AvdRESH),
                    fill = list(verdi_kar = "nei")) %>%

    # format wide med en rad per variabel:
    tidyr::pivot_wider(names_from = .data$kar,
                       values_from = .data$verdi_kar) %>%

    # Rekkefølge nye variabler, og nytt navn
    dplyr::select(.data$AvdRESH,
                  .data$ForlopsID,
                  .data$LMS,
                  .data$LAD,
                  .data$RCA,
                  .data$CX,
                  .data$Graft) %>%
    dplyr::rename_with(.data = .,
                       .fn = function(x) paste0(metodeType, "_", x),
                       .cols =  .data$LMS:.data$Graft)


  # Returnere df_ap, hvor de 5 nye variablene er lagt til.
  # Forløp i AP som ikke har rader i AD, får verdien NA for de nye kolonnene.
  dplyr::left_join(df_ap,
                   ad_wide,
                   by = c("AvdRESH", "ForlopsID"))

}








#' @rdname utlede_annenDiag_variabler
#' @export
legg_til_trykkmaalinger <- function(df_ap, df_ad) {



  . <- ""
  # Must contain matching-variables + variables needed for calculations
  stopifnot(all(c("ForlopsID", "AvdRESH", "metode") %in% names(df_ad)))


  # Must contain matching-variables + variables needed for calculations
  stopifnot(all(c("ForlopsID", "AvdRESH") %in% names(df_ap)))




  # Count number of entries with metode == "IMR" for each procedure, if at
  # least one entry, say yes to method.
  ant_imr <- df_ad %>%
    dplyr::select(.data$ForlopsID,
                  .data$AvdRESH,
                  .data$metode) %>%
    dplyr::arrange(., .data$AvdRESH)  %>%
    dplyr::group_by(.data$AvdRESH) %>%
    dplyr::count(.data$ForlopsID,
                 wt = .data$metode == "IMR") %>%
    dplyr::mutate(IMR = ifelse(test = .data$n > 0,
                               yes = "Ja",
                               no = "Nei")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$n)



  ant_pdpa <- df_ad %>%
    dplyr::select(.data$ForlopsID,
                  .data$AvdRESH,
                  .data$metode) %>%
    dplyr::arrange(., .data$AvdRESH)  %>%
    dplyr::group_by(.data$AvdRESH) %>%
    dplyr::count(.data$ForlopsID,
                 wt = .data$metode == "Pd/Pa") %>%
    dplyr::mutate(PdPa = ifelse(test = .data$n > 0,
                                yes = "Ja",
                                no = "Nei")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$n)

  ant_pa <- df_ad %>%
    dplyr::select(.data$ForlopsID,
                  .data$AvdRESH,
                  .data$metode) %>%
    dplyr::arrange(., .data$AvdRESH)  %>%
    dplyr::group_by(.data$AvdRESH) %>%
    dplyr::count(.data$ForlopsID,
                 wt = .data$metode == "Pa-hyperemi") %>%
    dplyr::mutate(Pa = ifelse(test = .data$n > 0,
                              yes = "Ja",
                              no = "Nei")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$n)


  ant_pd <- df_ad %>%
    dplyr::select(.data$ForlopsID,
                  .data$AvdRESH,
                  .data$metode) %>%
    dplyr::arrange(., .data$AvdRESH)  %>%
    dplyr::group_by(.data$AvdRESH) %>%
    dplyr::count(.data$ForlopsID,
                 wt = .data$metode == "Pd-hyperemi") %>%
    dplyr::mutate(Pd = ifelse(test = .data$n > 0,
                              yes = "Ja",
                              no = "Nei")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-n)




  # Add new variables to df_ap before returning df_ap
  df_ap %>%
    dplyr::left_join(.,
                     ant_imr,
                     by = c("AvdRESH", "ForlopsID")) %>%
    dplyr::left_join(.,
                     ant_pdpa,
                     by = c("AvdRESH", "ForlopsID")) %>%
    dplyr::left_join(.,
                     ant_pa,
                     by = c("AvdRESH", "ForlopsID")) %>%
    dplyr::left_join(.,
                     ant_pd,
                     by = c("AvdRESH", "ForlopsID")) %>%
    dplyr::arrange(.data$AvdRESH, .data$ForlopsID)

}
