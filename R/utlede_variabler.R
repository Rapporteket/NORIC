#' Add variable aldersklasse in decades
#'
#' Groups together ages in age-classes of decades, p.ex. "50-59" where ages 50
#' and 59 are min and max values of the age-class. Ages from 18 to 49 are
#' grouped together, whereas ages under 18 years or above 99 years are given
#' age-class <NA>.
#'
#' @param df data.frame, must contain a column with age as a continuous
#' variable
#' @param var is the variable name of the age-variable in \code{df}. Default
#' value is \code{PasientAlder}.
#' @return The input data.frame \code{df} is returned as it is,  with one new
#'  column named \code{aldersklasse} added.
#'
#' @export
#' @examples
#' df <-  data.frame(age = c(10,20,59,60, NA, 61,69,70,99,100))
#' utlede_aldersklasse(df = df, var = age)
utlede_aldersklasse <- function(df, var = PasientAlder) {
  df %>%
    dplyr::mutate(
      aldersklasse = cut(
        tidyr::replace_na({{var}}, replace = 0),
        breaks = c(18, 49, 59, 69, 79, 89, 99),
        include.lowest = TRUE,
        labels = c("18-49", "50-59", "60-69", "70-79", "80-89", "90-99"),
        ordered_result = TRUE)) %>%
    dplyr::relocate(.data$aldersklasse, .after = {{ var }})

}

#' Add variable OppholdsID
#'
#' Procedures that belongs to the same hospital-stay are given the same ID.
#'
#' @param df data.frame med Angio PCI data
#'
#' @return returns \code{df} with a new column \code{OppholdsID}. For
#' primaerforlop \code{OppholdsID} are copied from \code{ForlopsID}, whereas
#' for sekundaerforlop \code{OppholdsID} are copied from
#'  \code{PrimaerForlopsID} aka \code{KobletForlopsID}.
#' \code{PrimaerForlopsID}.
#'
#' @export
#'
#' @examples
#' x <- data.frame(
#'    Regtype = c(rep("Primær", 4), "Sekundær", "Sekundær"),
#'    ForlopsID = c(1, 2, 3, 4, 5, 6),
#'    PrimaerForlopsID = c(rep(NA, 4), 1, 3))
#' utlede_OppholdsID(x)
utlede_OppholdsID <- function(df){

  stopifnot(c("Regtype", "ForlopsID", "PrimaerForlopsID") %in% names(df))

  df %>%
    dplyr::mutate(OppholdsID = ifelse(.data$Regtype == "Primær",
                                      yes = .data$ForlopsID,
                                      no = .data$PrimaerForlopsID))

}

#' Add binary variable for table-status
#'
#' The function \code{utlede_ferdigstilt()} uses the values of variable
#' \code{var} to create a new column named \code{ferdigstilt_suffix} in
#' \code{df} before \code{df} is returned from the function. The new variable is
#' given the value \emph{ja} if \code{var} is 1, and value \emph{nei} if
#' \code{var} is -1 or 0.
#'
#' @param df data.frame, must contain a numerical status-variable with values
#'  -1, 0 or 1.
#' @param var variable name of the numerical status-variable. Default value
#' is \code{SkjemaStatusStart}.
#' @param suffix wanted suffix in the new variable's name:
#' \code{ferdigstilt_suffix}. Default values is "startSkjema".
#' @return The data.frame given in input \code{df} is returned as it is,
#' with one new column added.
#'
#' @export
#' @examples
#' x <- data.frame(SkjemaStatusStart = c(-1,-1,0,0,1,-1,0,1,NA),
#'                 SkjemastatusHovedskjema = rep(0,9),
#'                 SkjemaStatusUtskrivelse = rep(-1,9),
#'                 SkjemaStatusKomplikasjoner = c(rep(-1,4), rep(1,3), NA, NA))
#'
#' utlede_ferdigstilt(x, var = SkjemaStatusStart, suffix = "startSkjema")
#'
#' utlede_ferdigstilt(x,
#'                    var = SkjemaStatusStart,
#'                    suffix = "startSkjema") %>%
#'    utlede_ferdigstilt(.,
#'                       var = SkjemastatusHovedskjema,
#'                       suffix = "hovedSkjema") %>%
#'    utlede_ferdigstilt(.,
#'                       var = SkjemaStatusKomplikasjoner,
#'                       suffix = "komplikSkjema")

utlede_ferdigstilt <- function(df,
                               var = .data$SkjemaStatusStart,
                               suffix = "startSkjema") {

  dplyr::mutate(
    .data = df,
    "ferdigstilt_{suffix}" := dplyr::if_else({{ var }} == 1,
                                             true = "ja",
                                             false = "nei",
                                             missing = NA_character_)) %>%
    dplyr::relocate(paste0("ferdigstilt_", suffix), .after = {{ var }})
}



#' Use values from two variables
#'
#' The function \code{slaa_sammen_variabler()} uses the values of two variables,
#' \code{var1} and \code{var2}, in dataset \code{df} to create a new variable
#' named \code{var_name} in the same dataset. \code{var_name} is first given the
#' value of \code{var1}, in rows where \code{var1} is missing the values of
#' \code{var2} are used. In rows where both \code{var1} and \code{var2} are
#' missing, \code{var_name} is also missing. If \code{slette_var1_var2} is TRUE,
#' \code{var1} and \code{var2} are deleted before \code{df} is returned.

#' @param df data.frame, must have variables \code{var1} and \code{var2}.
#' @param var1 variable to be used as main-source for \code{var_name}
#' @param var2 variable to use when rows of \code{var1} are missing
#' @param var_name string with name of new variable
#' @param slette_gamle boolean. Should \code{var1} and \code{var2} be
#' deleted from \code{df} before \code{df} is returned? Default value is FALSE.
#'
#' @export
#'
#' @examples
#' x <- data.frame(var1 = c(rep("A", 5), NA, NA),
#'                 var2 = c(NA, rep("B", 5), NA))
#' slaa_sammen_variabler(df = x, var1 = var1, var2 = var2,
#'                       var_name = "var_1_2", slette_gamle = FALSE)
slaa_sammen_variabler <- function(df,
                                  var1, var2, var_name,
                                  slette_gamle = FALSE) {


  if (class(df %>% dplyr::pull({{ var1 }})) !=
      class(df %>% dplyr::pull({{ var2 }}))) {
    stop("maa vaere samme klasse")
  }


  df %<>%
    dplyr::mutate(
      "{var_name}" := dplyr::if_else(condition = is.na({{ var1 }}),
                                     true = {{ var2 }},
                                     false = {{ var1 }})) %>%
    # Sette på riktig plass i tabellen
    dplyr::relocate(var_name, .after = {{ var2 }})

  # Eventuelt slette gamle variabler
  if (slette_gamle)  df %<>% dplyr::select(- {{var1}}, - {{ var2 }})

  df
}


