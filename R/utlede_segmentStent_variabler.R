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
