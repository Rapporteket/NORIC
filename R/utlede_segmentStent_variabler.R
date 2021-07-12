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





# Utlede  PCI ja/nei per kar/graft
# Wireforsøk teller ikke
# Format Wide : En kolonne per kar + en kolonne per kar per Graft
ss %>% filter(ProsedyreType != "Wireforsøk") %>% count(kar_graftKar)

ss_wide_pci <- ss %>%
  arrange(AvdRESH, ForlopsID, kar_graftKar) %>%
  filter(ProsedyreType!= "Wireforsøk") %>%
  select(-Segment, -Graft, -ProsedyreType, -StentType, -kar_graft, -Sykehusnavn ) %>%
  group_by(ForlopsID, AvdRESH, kar_graftKar) %>%
  mutate(pci_kar = ifelse(
    test = n()>0,
    yes = "ja",
    no = "nei")) %>%
  distinct() %>%
  pivot_wider(names_from = kar_graftKar,
              values_from = pci_kar,
              values_fill = "nei") %>%
  # Disse nivåene mangler:
  mutate(LAD_arterieGraft = "nei",
         RCA_arterieGraft = "nei",
         CX_arterieGraft = "nei") %>%
  select(AvdRESH, ForlopsID, LMS, LAD, RCA, CX,
         LAD_arterieGraft, RCA_arterieGraft, CX_arterieGraft,
         LAD_veneGraft, RCA_veneGraft, CX_veneGraft) %>%
  rename_at(vars(LMS:CX_veneGraft), function(x) paste0("PCI_", x))


# Utlede variabelen WireforsøK per kar
ss %>% filter(ProsedyreType == "Wireforsøk") %>% count(kar_graftKar)

ss_wide_wire <- ss %>%
  arrange(AvdRESH, ForlopsID, kar_graftKar) %>%
  filter(ProsedyreType== "Wireforsøk") %>%
  select(-Segment, -Graft, -ProsedyreType, -StentType, -kar_graft, -Sykehusnavn ) %>%
  group_by(ForlopsID, AvdRESH, kar_graftKar) %>%
  mutate(wire_kar = ifelse(
    test = n()>0,
    yes = "ja",
    no = "nei")) %>%
  distinct() %>%
  pivot_wider(names_from = kar_graftKar,
              values_from = wire_kar,
              values_fill = "nei") %>%
  mutate(LMS = "nei",
         LAD_arterieGraft = "nei",
         CX_arterieGraft = "nei") %>%
  select(AvdRESH, ForlopsID, LMS, LAD, RCA, CX,
         LAD_arterieGraft, RCA_arterieGraft, CX_arterieGraft,
         LAD_veneGraft, RCA_veneGraft, CX_veneGraft) %>%
  rename_at(vars(LMS:CX_veneGraft), function(x) paste0("wireforsok_", x))



# kontroll
ss %>% filter(ForlopsID %in% c(4227, 8902, 1674, 6954, 4136 )) %>% print(n=20)
ss_wide_pci %>% filter(ForlopsID %in% c(4227, 8902, 1674, 6954, 4136 ))
ss_wide_wire %>% filter(ForlopsID %in% c(4227, 8902, 1674, 6954 , 4136))


```


# ##  Legge til variabler fra Segment Stent i AngioPCI. Dersom SegmentStent mangler
# for noen forløp blir verdien NA.
# ```{r merge_ap_ss}
# ap_uttrekk_ss <- ap_uttrekk %>%
#   left_join(.,
#             ss_wide_pci,
#             by = c("ForlopsID", "AvdRESH")) %>%
#   left_join(.,
#             ss_wide_wire,
#             by = c("ForlopsID", "AvdRESH"))
#
# dim(ap_uttrekk_ss)
# names(ap_uttrekk_ss)
#
#
# ```






