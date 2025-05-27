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
utlede_OppholdsID <- function(df) {

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



#' Minst en registrering av død under forløpet
#'
#'Funksjonen \code{utlede_dod_noric} genererer en ny variabel i AP-datasettet:
#' \code{dod_noric}. Dersom minst en av variablene
#'\code{LabKompDod}, \code{AvdKompDod} eller \code{UtskrevetDod} har verdien
#'\emph{Ja}, eller \code{UtskrevetDodsdato} finnes, så regnes pasienten som
#'registrert død under forløpet. Gjelder alle typer forløp, primær og sekundær.
#'Brukes sammen med funksjonen \code{avdod_opphold} for å gruppere
#'NORIC-opphold sammen.
#'
#' @param df_ap data.frame med AngioPCI-data tabellen. Må inneholde variablene
#'\code{LabKompDod}, \code{AvdKompDod}, \code{UtskrevetDod} og
#' \code{UtskrevetDodsdato}.
#' @return Funksjonen returnerer \code{ap_df}, med en ny kolonne
#' ved navn \code{dod_noric}.
#'
#' @examples
#' x <- data.frame(LabKompDod = c("Ja", "Nei", "Ukjent"),
#'                 AvdKompDod = rep(NA_character_, 3),
#'                 UtskrevetDod = c("Nei", "Ja", "Nei"),
#'                 UtskrevetDodsdato = as.Date(c(NA_character_,
#'                                               "2021-01-01",
#'                                               NA_character_),
#'                                               format = "%Y-%m-%d"))
#' noric::utlede_dod_noric(x)
#' @export
utlede_dod_noric <- function(df_ap){
  stopifnot(all(c("LabKompDod",
                  "AvdKompDod",
                  "UtskrevetDod",
                  "UtskrevetDodsdato") %in% names(df_ap)))


  df_ap %>%
    dplyr::mutate(
      dod_noric = dplyr::if_else(
        condition = (.data$AvdKompDod %in% "Ja" |
                       .data$LabKompDod %in% "Ja" |
                       .data$UtskrevetDod %in% "Ja" |
                       !is.na(.data$UtskrevetDodsdato)),
        true = "Ja",
        false = "Nei",
        missing = "Nei"))

}




#' Minst en gang død under NORIC-oppholdet
#'
#' Funksjonen grupperer primær- og sekundærforløp sammen, ved hjelp
#' av \code{OppholdsID}. En ny variabel blir generert i AP-tabellen:
#' \code{dod_opphold}. Denne variabelen får verdien \emph{Ja} dersom
#' \code{dod_noric} har verdien \emph{Ja} for minst et av forløpene med felles
#' \code{OppholdsID} og får verdien \emph{Nei} dersom ingen av forløpene
#' med felles \code{OppholdsID} har registrering av avdød. Funksjonen brukes
#' sammen med \code{noric::utlede_OppholdsID} og
#' \code{noric::utlede_dod_noric}.
#'
#' @param df_ap data.frame med AngioPCI-data tabellen. Må inneholde variablene
#'\code{AvdRESH}, \code{OppholdsID} og \code{dod_noric}.
#' @return Funksjonen returnerer \code{ap_df}, med en ny kolonne
#' ved navn \code{dod_opphold}.
#'
#' @examples
#' x <- data.frame(AvdRESH = rep(123456, 8),
#'                 OppholdsID = c(1, 1, 1, 2, 3, 3, 4, 5),
#'                 dod_noric = c("Nei", "Nei", "Ja", "Nei",
#'                               "Nei", "Nei", "Ja", "Nei"))
#' noric::avdod_opphold(x)
#'
#'x <- data.frame(AvdRESH = rep(123456, 6),
#'                ForlopsID = 1:6,
#'                Regtype = c(rep("Primær", 4), "Sekundær", "Sekundær"),
#'                PrimaerForlopsID = c(1:4, 4, 4),
#'                LabKompDod = c("Ja", rep("Nei", 4), "Ja"),
#'                AvdKompDod = c("Nei", "Nei", "Ja", "Nei",
#'                              NA_character_, NA_character_),
#'                UtskrevetDod = c(rep("Nei", 4),
#'                                 NA_character_, NA_character_),
#'                UtskrevetDodsdato = rep(NA_character_, 6))
#' x %>%
#'   noric::utlede_OppholdsID(.) %>%
#'   noric::utlede_dod_noric(.) %>%
#'   noric::avdod_opphold(.)
#'
#'
#' @export
avdod_opphold <- function(df_ap) {

  stopifnot(all(c("AvdRESH",
                  "OppholdsID",
                  "dod_noric") %in% names(df_ap)))

  df_ap %>%
    dplyr::group_by(.data$AvdRESH, .data$OppholdsID) %>%
    dplyr::mutate(
      dod_opphold = ifelse(
        all(.data$dod_noric == "Nei"),
        "Nei",
        "Ja")) %>%
    dplyr::ungroup()

}
