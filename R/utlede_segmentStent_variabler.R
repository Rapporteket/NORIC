#' New variables from NORIC's SementStent-table
#'
#' Functions for creating variables based on NORIC's \emph{SegmentStent}-table
#' and for merging these variables into NORIC's \emph{AngioPCIVar}-table by key
#' variables \code{ForlopsID} and \code{AvdRESH}.
#'
#' The function \code{legg_til_antall_stent()} counts the number of non-missing
#' stent-types for each procedure in \code{df_ss}. The variable
#'  \code{antall_stent} is then added in \code{df_ap} before \code{df_ap} is
#'  returned. For procedures with no entries in \code{df_ss},
#'  \code{antall_stent} has the value <NA>.
#'
#' The function \code{legg_til_antall_stent_opphold()} groups the
#' procedures (primErforløp - koblet primaerforløp) by main procedure and
#' counts the total number of stents. If no registration in SegmentStent-table
#' for any procedure associated with a main procedure, NA is returned. If at
#' least one registration is available in SegmentStent, the sum om stents is
#' returned (the sum might be 0, if only other types of interventions were
#'  registered).
#'
#' The function \code{utlede_kar_segment_stent()} groups the
#' \code{Segments} in \code{df_ss} into coronary arteries in the new variable
#' \code{kar}, then \code{df_ss} is returned from the function.
#'
#' The function \code{utlede_kar_graft_segment_stent()} groups the
#' \code{Segments} and \code{Grafts} in \code{df_ss} into coronary arteries
#' and/or grafts in the new variable \code{kar_graft}, then \code{df_ss} is
#' returned from the function.
#'
#' The function \code{satt_inn_stent_i_lms()} adds the variable
#' \code{satt_inn_stent_i_LMS} in table \code{df_ap}. The variable is binary
#' and has value \emph{ja} if at least one stent is registered in LMS for the
#' procedure, value \emph{nei} if 0 stents are registered in LMS and finally
#'  value \emph{NA} if no rows are available in \code{df_ss} for the procedure.

#'
#' The function \code{legg_til_pci_per_kar()} uses the data in \code{df_ss} to
#' calculate 10 new variables. Variable names are prefixed by \code{PCI_} and
#' suffixed by level of \code{kar_graft} (p.ex.: \code{PCI_LAD}, \code{PCI_CX},
#' \code{PCI_CX_veneGraft}, \code{PCI_CX_arterieGraft}). The variables indicates
#' whether or not a PCI is performed in each \code{kar_graft}. If at least one
#' PCI is done in a \code{kar_graft} the value of the corresponding variable is
#' \emph{ja}, otherwise the value is \emph{nei}. These 10 variables are then
#' merged into \code{df_ap}, before \code{df_ap} is returned from the function.
#' For procedures where no PCI's are done on segment-level (e.g. no entries
#' available in \code{df_ss}), the values for the 10 new variables are <NA>.
#' Procedures of type \emph{wireforsoek} are not counted.
#'
#'
#' The function \code{legg_til_wireforsok_per_kar()} uses the data in
#' \code{df_ss} to calculate 10 new variables. Variable names are prefixed by
#' \code{wireforsok_} and suffixed by level of \code{kar_graft}
#' (p.ex.: \code{wireforsok_LAD}, \code{wireforsok_CX},
#' \code{wireforsok_CX_veneGraft}, \code{wireforsok_CX_arterieGraft}).
#' The variables indicates whether or not a wireforsøk is performed in each
#' \code{kar_graft}. If at least one wireforsøk is done in a \code{kar_graft}
#' the value of the corresponding variable is \emph{ja}, otherwise the value is
#' \emph{nei}. These 10 variables are then merged into \code{df_ap}, before
#' \code{df_ap} is returned from the function. For procedures where no
#' wireforsok's are done on segment-level (e.g. no entries
#' available in \code{df_ss}), the values for the 10 new variables are <NA>.
#' Only procedures of type \emph{wireforsoek} are counted.

#'
#'
#' @param df_ap NORICS's \emph{AngioPCIVar}-table. Must contain the variables
#'  \code{AvdRESH} and \code{ForlopsID}.
#'
#' @param df_ss NORICS's \emph{SegmentStent}-table. Must contain the variables
#' \code{AvdRESH} and \code{ForlopsID}. Additionally, the variable
#' \code{StentType} is mandatory in the function \code{legg_til_antall_stent()}
#' and the variables \code{Segment}, \code{Graft} and \code{ProsedyreType} are
#' mandatory in the functions \code{utlede_kar_segment_stent()},
#' \code{utlede_kar_graft_segment_stent()}, \code{legg_til_pci_per_kar()} and
#' \code{legg_til_wireforsok_per_kar()}.
#'
#' @name utlede_segmentStent_variabler
#' @aliases
#' legg_til_antall_stent
#' legg_til_antall_stent_opphold
#' utlede_kar_segment_stent
#' utlede_kar_graft_segment_stent
#' satt_inn_stent_i_lms
#' legg_til_pci_per_kar
#' legg_til_wireforsok_per_kar
#'
#'
#'
#' @examples
#' df_ap <- data.frame(ForlopsID = 1:5,
#'                     AvdRESH = rep(1,5))
#'
#' # Legg til antall stent i df_ap
#' df_ss <- data.frame(
#'   ForlopsID = c(1, 3, 3, 3, 5, 5),
#'   AvdRESH = rep(1,6),
#'   Segment = 1:6,
#'   StentType = c("DES",
#'                 "BMS", "DES", "Annet",
#'                 NA, NA))
#' legg_til_antall_stent(df_ap = df_ap, df_ss = df_ss)
#'
#'
#' x <- data.frame(AvdRESH = rep(1, 13),
#'                 OppholdsID = c(101:106, 101, 102, 102, 103, 104, 106, 50),
#'                 antall_stent = c(0, 5, NA, 1, NA, NA,
#'                                  3, 1, 2, 3, NA, NA, 10))
#' noric::legg_til_antall_stent_opphold(x)
#'
#'
#'
#' # Legg til kar ellerkar_graft i df_ss
#' df_ss <- data.frame(ForlopsID = 1:23,
#'                     AvdRESH = rep(1,23),
#'                     Segment = c(1:20, 1:3),
#'                     Graft=c(rep("Nei", 20), "Arteriell", "Vene", NA))
#' utlede_kar_segment_stent(df_ss)
#' utlede_kar_graft_segment_stent(df_ss)
#'
#'
#' # Legg til PCI-variabler og wireforsok-variabler per kar_graft i df_ap
#' df_ss <- data.frame(ForlopsID = c(1,2,3,3,3),
#'                     AvdRESH = rep(1,5),
#'                     Segment = c(1,5,10,12,13),
#'                     Graft = c(rep("Nei", 3),
#'                               rep("Arteriell", 1),
#'                               rep("Vene", 1)),
#'                     ProsedyreType = c("Ballong + Stent",
#'                                       "Wireforsøk",
#'                                       "Rotablator",
#'                                       "Wireforsøk",
#'                                       "Direktestent"))
#' legg_til_pci_per_kar(df_ap = df_ap, df_ss = df_ss)
#' legg_til_wireforsok_per_kar(df_ap = df_ap, df_ss = df_ss)

NULL

#' @rdname utlede_segmentStent_variabler
#' @export
legg_til_antall_stent <- function(df_ap, df_ss) {
  . <- ""

  if (!all(c("AvdRESH", "ForlopsID") %in% names(df_ap))) {
    stop("df_apmust contain variables AvdRESH + ForlopsID")
  }

  if (!all(c("AvdRESH", "ForlopsID", "StentType") %in% names(df_ss))) {
    stop("ss must contain variables AvdRESH + ForlopsID + StentType")
  }

  # Count number of non-missing entries in StentType for each procedure
  ant_stent <- df_ss %>%
    dplyr::select(.data$AvdRESH, .data$ForlopsID, .data$StentType) %>%
    dplyr::arrange(., .data$AvdRESH)  %>%
    dplyr::group_by(.data$AvdRESH) %>%
    dplyr::count(.data$ForlopsID,
                 wt = !is.na(.data$StentType)) %>%
    dplyr::rename("antall_stent" = .data$n)

  # Add new variable to df_ap before returning df_ap
  dplyr::left_join(df_ap,
                   ant_stent,
                   by = c("AvdRESH", "ForlopsID")) %>%
    dplyr::arrange(.data$AvdRESH, .data$ForlopsID)
}




#' @rdname utlede_segmentStent_variabler
#' @export
legg_til_antall_stent_opphold <- function(df_ap) {

  stopifnot(all(c("AvdRESH",
                  "OppholdsID",
                  "antall_stent") %in% names(df_ap)))

  # Antall stent satt inn under hvert opphold:
  df_ap %>%

    # Gruppere oppholdene sammen
    dplyr::group_by(.data$AvdRESH, .data$OppholdsID) %>%

    # Antall stent satt inn under opphold
    # Dersom ingen informajon i SS for noen av forløpene for oppholdet -->NA
    # Dersom minst en informasjon i SS (selv om dette er "0 stent") --> summen
    dplyr::mutate(
      antall_stent_under_opphold = ifelse(
        all(is.na(.data$antall_stent)),
        NA,
        sum(.data$antall_stent, na.rm = TRUE))) %>%

    dplyr::ungroup()

}




#' @rdname utlede_segmentStent_variabler
#' @export
satt_inn_stent_i_lms <- function(df_ap, df_ss) {

  . <- ""

  # Must contain matching-variables + variables needed for calculations
    if (!all(c("ForlopsID", "AvdRESH", "Segment", "Graft", "StentType") %in%
             names(df_ss))) {
      stop("df_ss must contain variables ForlopsID, AvdRESH, Segment, Graft and
         StentType")
    }

    # Must contain matching-variables + variables needed for calculations
    if (!all(c("ForlopsID", "AvdRESH") %in%
             names(df_ap))) {
      stop("df_ap must contain variables ForlopsID and AvdRESH")
    }

    ss_wide_stent_lms <- df_ss %>%

      # Legge til variabel kar
      noric::utlede_kar_segment_stent(.) %>%
      dplyr::select(.data$ForlopsID,
                    .data$AvdRESH,
                    .data$kar,
                    .data$StentType) %>%
      dplyr::arrange(.data$AvdRESH, .data$ForlopsID, .data$kar) %>%

      # Teller om minst en stent ble satt inn i LMS
      # Dersom 0 stent i karet  n=0 --> "nei"
      # Dersom minst en stent i karet blir verdien n > 0 --> "ja"
      dplyr::count(.data$AvdRESH, .data$ForlopsID, .data$kar,
                   wt = !is.na(.data$StentType)) %>%
      dplyr::mutate(stent_i_kar = ifelse(
        test = .data$n > 0,
        yes = "ja",
        no = "nei")) %>%
      dplyr::select(- .data$n) %>%
      dplyr::distinct() %>%

      # For alle kombinasjoner av ForlopsID og AvdRESH som har minst en rad i
      # datasettet SS (finner dem med funksjonen nesting),
      # komplettes manglende nivåer av kar med verdien "nei"
      tidyr::complete(.data$kar,
                      tidyr::nesting(ForlopsID, AvdRESH),
                      fill = list(stent_i_kar = "nei")) %>%

      # format med en rad per variabel:
      tidyr::pivot_wider(names_from = .data$kar,
                         values_from = .data$stent_i_kar) %>%

      # Beholde bare LMS, fjerne de andre karene
      dplyr::select(.data$AvdRESH,
                    .data$ForlopsID,
                    .data$LMS) %>%

      dplyr::rename("satt_inn_stent_i_LMS" = .data$LMS)


    # Legg til 1 ny variablel i AP : stent_i_LMS = ja/nei/NA.
    # Forløp i AP som ikke har rader i SS, vil få verdien NA .
    dplyr::left_join(df_ap,
                     ss_wide_stent_lms,
                     by = c("AvdRESH", "ForlopsID"))

  }



#' @rdname utlede_segmentStent_variabler
#' @export
utlede_kar_segment_stent <- function(df_ss) {


  # Must contain matching-variables + variables needed for calculations
  if (!all(c("ForlopsID", "AvdRESH", "Segment", "Graft") %in% names(df_ss))) {
    stop("df must contain variables ForlopsID, AVdRESH, Segment and Graft")
  }

  dplyr::mutate(
    df_ss,

    kar = factor(dplyr::case_when(
      .data$Graft %in% c("Arteriell", "Vene") ~ "Graft",
      .data$Segment %in% c(1, 2, 3, 4, 18, 19) ~ "RCA",
      .data$Segment == 5 ~ "LMS",
      .data$Segment %in% c(6, 7, 8, 9, 10, 20) ~ "LAD",
      .data$Segment %in% c(11, 12, 13, 14, 15, 16, 17) ~ "CX"),
      levels = c("LMS",
                 "LAD",
                 "RCA",
                 "CX",
                 "Graft")))
}




#' @rdname utlede_segmentStent_variabler
#' @export
utlede_kar_graft_segment_stent <- function(df_ss) {


  # Must contain matching-variables + variables needed for calculations
  if (!all(c("ForlopsID", "AvdRESH", "Segment", "Graft") %in% names(df_ss))) {
    stop("df must contain variables ForlopsID, AVdRESH, Segment and Graft")
  }


  dplyr::mutate(
    df_ss,

    kar_graft = factor(dplyr::case_when(
      .data$Segment %in% c(1, 2, 3, 4, 18, 19) &
        .data$Graft == "Nei" ~ "RCA",
      .data$Segment == 5 &
        .data$Graft == "Nei" ~ "LMS",
      .data$Segment %in% c(6, 7, 8, 9, 10, 20) &
        .data$Graft == "Nei"~ "LAD",
      .data$Segment %in% c(11, 12, 13, 14, 15, 16, 17) &
        .data$Graft == "Nei"~ "CX",

      .data$Segment %in% c(1, 2, 3, 4, 18, 19) &
        .data$Graft == "Arteriell" ~ "RCA_arterieGraft",
      .data$Segment == 5 &
        .data$Graft == "Arteriell" ~ NA_character_,
      .data$Segment %in% c(6, 7, 8, 9, 10, 20) &
        .data$Graft == "Arteriell"~ "LAD_arterieGraft",
      .data$Segment %in% c(11, 12, 13, 14, 15, 16, 17) &
        .data$Graft == "Arteriell"~ "CX_arterieGraft",

      .data$Segment %in% c(1, 2, 3, 4, 18, 19) &
        .data$Graft == "Vene" ~ "RCA_veneGraft",
      .data$Segment == 5 &
        .data$Graft == "Vene" ~ NA_character_,
      .data$Segment %in% c(6, 7, 8, 9, 10, 20) &
        .data$Graft == "Vene" ~ "LAD_veneGraft",
      .data$Segment %in% c(11, 12, 13, 14, 15, 16, 17) &
        .data$Graft == "Vene" ~ "CX_veneGraft"),

      levels = c("LMS",
                 "LAD",
                 "RCA",
                 "CX",
                 "LAD_arterieGraft",
                 "RCA_arterieGraft",
                 "CX_arterieGraft",
                 "LAD_veneGraft",
                 "RCA_veneGraft",
                 "CX_veneGraft")))
}




#' @rdname utlede_segmentStent_variabler
#' @export
legg_til_pci_per_kar <- function(df_ap, df_ss) {

  . <- ""

  # Must contain matching-variables + variables needed for calculations
  if (!all(c("ForlopsID", "AvdRESH", "Segment", "Graft", "ProsedyreType") %in%
           names(df_ss))) {
    stop("df_ss must contain variables ForlopsID, AvdRESH, Segment Graft
         and ProsedyreType")
  }

  # Must contain matching-variables + variables needed for calculations
  if (!all(c("ForlopsID", "AvdRESH") %in%
           names(df_ap))) {
    stop("df_ap must contain variables ForlopsID and AvdRESH")
  }

  ss_wide_pci <- df_ss %>%

    # Legge til variabel kar_graft
    noric::utlede_kar_graft_segment_stent(.) %>%
    dplyr::select(.data$ForlopsID,
                  .data$AvdRESH,
                  .data$kar_graft,
                  .data$ProsedyreType) %>%
    dplyr::arrange(.data$AvdRESH, .data$ForlopsID, .data$kar_graft) %>%

    # Fjerner Wireforsøk og teller alle andre PCI-prosedyrer per kar
    # Dersom 0 prosedyrer i karet (Kun wireforsøk) blir verdien n=0 --> "nei"
    # Dersom minst en prosedyre i karet blir verdien n > 0 --> "ja"
    dplyr::count(.data$AvdRESH, .data$ForlopsID, .data$kar_graft,
                 wt = .data$ProsedyreType != "Wireforsøk") %>%
    dplyr::mutate(pci_kar = ifelse(
      test = .data$n > 0,
      yes = "ja",
      no = "nei")) %>%
    dplyr::select(- .data$n) %>%
    dplyr::distinct() %>%

    # For alle kombinasjoner av ForlopsID og AvdRESH som har minst en rad i
    # datasettet SS (finner dem med funksjonen nesting),
    # komplettes manglende nivåer av kar_graft med verdien "nei"
    tidyr::complete(.data$kar_graft,
                    tidyr::nesting(ForlopsID, AvdRESH),
                    fill = list(pci_kar = "nei")) %>%

    # format med en rad per variabel:
    tidyr::pivot_wider(names_from = .data$kar_graft,
                       values_from = .data$pci_kar) %>%

    # Rekkefølge nye variabler, og nytt navn
    dplyr::select(.data$AvdRESH,
                  .data$ForlopsID,
                  .data$LMS,
                  .data$LAD,
                  .data$RCA,
                  .data$CX,
                  .data$LAD_arterieGraft,
                  .data$RCA_arterieGraft,
                  .data$CX_arterieGraft,
                  .data$LAD_veneGraft,
                  .data$RCA_veneGraft,
                  .data$CX_veneGraft) %>%
      dplyr::rename_with(.data = .,
                       .fn = function(x) paste0("PCI_", x),
                       .cols =  .data$LMS:.data$CX_veneGraft)



  # Legg til 10 nye variabler i AP. Forløp i AP som ikke har rader i SS,
  # vil få verdien NA for de nye kolonnene.
  dplyr::left_join(df_ap,
                   ss_wide_pci,
                   by = c("AvdRESH", "ForlopsID"))

}



#' @rdname utlede_segmentStent_variabler
#' @export
legg_til_wireforsok_per_kar <- function(df_ap, df_ss) {

  . <- ""

  # Must contain matching-variables + variables needed for calculations
  if (!all(c("ForlopsID", "AvdRESH", "Segment", "Graft", "ProsedyreType") %in%
           names(df_ss))) {
    stop("df_ss must contain variables ForlopsID, AvdRESH, Segment Graft
         and ProsedyreType")
  }

  # Must contain matching-variables + variables needed for calculations
  if (!all(c("ForlopsID", "AvdRESH") %in%
           names(df_ap))) {
    stop("df_ap must contain variables ForlopsID and AvdRESH")
  }

  ss_wide_wire <- df_ss %>%

    # Legge til variabel kar_graft
    utlede_kar_graft_segment_stent(.) %>%
    dplyr::select(.data$ForlopsID,
                  .data$AvdRESH,
                  .data$kar_graft,
                  .data$ProsedyreType) %>%
    dplyr::arrange(.data$AvdRESH, .data$ForlopsID, .data$kar_graft) %>%

    # Teller kun wireforsok,per kar
    # Dersom 0 wireforsøk i karet blir verdien n=0 --> "nei"
    # Dersom minst et wireforsøk i karet blir verdien n > 0 --> "ja"
    dplyr::count(.data$AvdRESH, .data$ForlopsID, .data$kar_graft,
                 wt = .data$ProsedyreType == "Wireforsøk") %>%
    dplyr::mutate(wire_kar = ifelse(
      test = .data$n > 0,
      yes = "ja",
      no = "nei")) %>%
    dplyr::select(- .data$n) %>%
    dplyr::distinct() %>%

    # For alle kombinasjoner av ForlopsID og AvdRESH som har minst en rad i
    # datasettet SS (finner dem med funksjonen nesting),
    # komplettes manglende nivåer av kar_graft med verdien "nei"
    tidyr::complete(.data$kar_graft,
                    tidyr::nesting(ForlopsID, AvdRESH),
                    fill = list(wire_kar = "nei")) %>%

    # format med en rad per variabel:
    tidyr::pivot_wider(names_from = .data$kar_graft,
                       values_from = .data$wire_kar) %>%

    # Rekkefølge nye variabler, og nytt navn
    dplyr::select(.data$AvdRESH,
                  .data$ForlopsID,
                  .data$LMS,
                  .data$LAD,
                  .data$RCA,
                  .data$CX,
                  .data$LAD_arterieGraft,
                  .data$RCA_arterieGraft,
                  .data$CX_arterieGraft,
                  .data$LAD_veneGraft,
                  .data$RCA_veneGraft,
                  .data$CX_veneGraft) %>%
    dplyr::rename_with(.data = .,
                       .fn = function(x) paste0("wireforsok_", x),
                       .cols =  .data$LMS:.data$CX_veneGraft)



  # Legg til 10 nye variabler i AP. Forløp i AP som ikke har rader i SS,
  # vil få verdien NA for de nye kolonnene.
  dplyr::left_join(df_ap,
                   ss_wide_wire,
                   by = c("AvdRESH", "ForlopsID"))

}
