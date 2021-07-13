#' Add number of stents per procedure in AP-dataset
#'
#' @param ap Angio PCI dataset where new variable `antall_stent` should be added.
#'  Must contain the variables `AvdRESH` and `ForlopsID`,
#' @param ss segment stent dataset where `antall_stent` is calculated from.
#' Must contain the variables `AvdRESH`, `ForlopsID` and `StentType`
#' @return data.frame with one new variable
#' @export
#'
#' @examples
#' \dontrun{
#' AP %>% legg_til_antall_stent(., ss = SS)
#' }
legg_til_antall_stent <- function(ap, ss){

  if (!all( c("AvdRESH" , "ForlopsID") %in% names(ap))){
    stop("ap must contain variables AvdRESH + ForlopsID")
  }

  if (!all( c("AvdRESH" , "ForlopsID", "StentType") %in% names(ss))){
    stop("ss must contain variables AvdRESH + ForlopsID + StentType")
  }

  message("Legger til variabelen antall_stent...")

    # In SS-dataset : Count number of non-missing entries in StentType for each
  # procedure
  ant_stent <- ss %>%
    dplyr::select(.data$AvdRESH, .data$ForlopsID, .data$StentType) %>%
    dplyr::arrange(., .data$AvdRESH)  %>%
    dplyr::group_by(.data$AvdRESH) %>%
    dplyr::count(.data$ForlopsID,
                 wt = !is.na(.data$StentType)) %>%
    rename("antall_stent" = .data$n)


  ap %>%
    dplyr::left_join(.,
                     ant_stent,
                     by = c("AvdRESH", "ForlopsID")) %>%
    arrange(.data$AvdRESH, .data$ForlopsID)
}






#' Add variable `kar` to NORIC segment-stent-table
#' Based on variables `Segment` and `Graft`
#'
#' @param df_ss segment-stent table, must contain variables `ForlopsID`,
#' `AVdRESH`, `Segment` and `Graft`
#'
#' @return segment-stent-table with one new variable
#' @export
#'
#' @examples
#' x <- data.frame(ForlopsID = 1:23,
#'                 AvdRESH = rep(1,23),
#'                 Segment = c(1:20, 1:3),
#'                 Graft=c(rep("Nei", 20), "Arteriell", "Vene", NA))
#' x %>% utlede_kar_segmentStent(.)
utlede_kar_segmentStent <- function(df = ss){


  # Must contain matching-variables + variables needed for calculations
  if(!all(c("ForlopsID", "AvdRESH", "Segment", "Graft") %in% names(df))) {
    stop("df must contain variables ForlopsID, AVdRESH, Segment and Graft")
  }

  df %>%
    dplyr::mutate(
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




#' Add variable `kar_graft` to NORIC segment-stent-table
#' Based on variables `Segment` and `Graft`. More detailed than
#' utlede_kar_segmentStent(), here also `Graft` is detailed for each level
#' of `kar`.
#'
#' @param df_ss segment-stent table, must contain variables `ForlopsID`,
#' `AVdRESH`, `Segment` and `Graft`
#'
#' @return segment-stent-table with one new variable
#' @export
#'
#' @examples
#' x <- data.frame(ForlopsID = 1:23,
#'                 AvdRESH = rep(1,23),
#'                 Segment = c(1:20, 1:3),
#'                 Graft=c(rep("Nei", 20), "Arteriell", "Vene", NA))
#' x %>% utlede_kar_graft_segmentStent(.)

utlede_kar_graft_segmentStent <- function(df = ss){


  # Must contain matching-variables + variables needed for calculations
  if(!all(c("ForlopsID", "AvdRESH", "Segment", "Graft") %in% names(df))) {
    stop("df must contain variables ForlopsID, AVdRESH, Segment and Graft")
  }

  df %>%
    dplyr::mutate(kar_graft = factor(case_when(
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




#' Add PCI variables per level of kar_graft in AP-data
#'
#' Add 10 new variables, all prefixed by "PCI_" and suffixed by level of
#' `kar_graft` (e.g. PCI_LAD, PCI_CX, PCI_CX_veneGraft, PCI_CX_arterieGraft).
#' Counts number of rows in Segment-stent data per procedure and per
#' level of `kar_graft`, removing "wireforsøk". If n=0 (only wireforsøk) all
#' 10 new variables are given value "nei". If n>1 for one or more levels for
#' `kar_graft` these variables are given value "ja", remaining variables are
#' given value "nei". In procedures with zero rows in stent-data, all new variables
#' are given value `NA`.
#'
#' @param df_ap AP data where new variables should be added
#' @param df_ss SS-data used to calculate 10 new variables
#'
#' @return `df_ap` with 10 new columns.
#' @export
#'
#' @examples
#'   ap <- data.frame(ForlopsID = 1:5,
#'                    AvdRESH = rep(1,5))
#'
#'   ss <- data.frame(ForlopsID = c(1,2,3,3,3),
#'                    AvdRESH = rep(1,5),
#'                    Segment = c(1,5,10,12,13),
#'                    Graft = c(rep("Nei", 3),
#'                              rep("Arteriell", 1),
#'                              rep("Vene", 1)),
#'                    ProsedyreType = c("Ballong + Stent",
#'                                      "Wireforsøk",
#'                                      "Rotablator",
#'                                      "Wireforsøk",
#'                                      "Direktestent"))
#'   ap %>% legg_til_pci_per_kar(., df_ss = ss)
legg_til_pci_per_kar <- function(df_ap, df_ss){

  # Must contain matching-variables + variables needed for calculations
  if(!all(c("ForlopsID", "AvdRESH", "Segment", "Graft", "ProsedyreType") %in%
          names(df_ss))) {
    stop("df_ss must contain variables ForlopsID, AvdRESH, Segment Graft and ProsedyreType" )
  }

  # Must contain matching-variables + variables needed for calculations
  if(!all(c("ForlopsID", "AvdRESH") %in%
          names(df_ap))) {
    stop("df_ap must contain variables ForlopsID and AvsRESH" )
  }

  ss_wide_pci <- df_ss %>%

    # Legge til variabel kar_graft
    utlede_kar_graft_segmentStent(.) %>%
    select(.data$ForlopsID,
           .data$AvdRESH,
           .data$kar_graft,
           .data$ProsedyreType) %>%
    arrange(.data$AvdRESH, .data$ForlopsID, .data$kar_graft) %>%

    # Fjerner Wireforsøk og teller alle andre PCI-prosedyrer per kar
    # Dersom ingen prosedyrer i karet (Kun wireforsøk) blir verdien n = 0 --> "nei")
    # Dersom minst en prosedyre i karet blir verdien n > 0 --> "ja"
    count(.data$AvdRESH, .data$ForlopsID, .data$kar_graft,
          wt = .data$ProsedyreType != "Wireforsøk") %>%
    mutate(pci_kar = ifelse(
      test = .data$n > 0,
      yes = "ja",
      no = "nei")) %>%
    select( - .data$n) %>%
    distinct() %>%

    # For alle kombinasjoner av ForlopsID og AvdRESH som har minst en rad i
    # datasettet SS (finner dem med funksjonen nesting),
    # komplettes manglende nivåer av kar_graft med verdien "nei"
    tidyr::complete(.data$kar_graft,
                    nesting(ForlopsID, AvdRESH),
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
    dplyr::rename_at(vars(.data$LMS:.data$CX_veneGraft),
                     function(x) paste0("PCI_", x))


  # Legg til 10 nye variabler i AP. Forløp i AP som ikke har rader i SS, vil få
  # verdien NA for de nye kolonnene.
  df_ap %>% dplyr::left_join(.,
                             ss_wide_pci,
                             by = c("AvdRESH", "ForlopsID"))

}

