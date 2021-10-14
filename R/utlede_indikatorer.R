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
#'  \item denominator \code{indik_komplik_ferdig_data} (datagrunnlag) is
#'  \emph{ja} when \emph{Regtype = Primaer}.
#'  \item numerator \code{indik_komplik_ferdig} has value \emph{ja} if the
#'  complications form is completed (komplikasjons-skjema er ferdigstilt).}
#'
#'
#'
#' \code{ki_trykkmaaling_utfoert()}
#' \itemize{
#' \item denominator \code{indik_trykkmaaling_data} (datagrunnlag) is \emph{ja}
#' when \emph{Indikasjon = Stabil koronarsykdom}.
#' \item numerator \code{indik_trykkmaaling} has value \emph{ja} if FFR and/or
#'  iFR has been performed.}
#'
#' \code{ki_ivus_oct_ved_stenting_lms()}
#' \itemize{
#' \item denominator \code{ki_ivus_oct_ved_stenting_lms_dg} (datagrunnlag) is
#' \emph{ja} when all of these conditions are fulfilled:
#' \enumerate{
#'   \item \code{Indikasjon} is one of "Vitieutredning", "Uklare brystsmerter",
#'  "Annet", "Hjertestans uten STEMI", "Hjertesvikt/kardiomyopati",
#'   "Komplettering av tidligere PCI", "UAP", "NSTEMI",
#'   "Stabil koronarsykdom"
#'   \item \code{satt_inn_stent_i_LMS} = "ja"
#'   \item \code{TidlABC} is one of "Nei", "Ukjent", NA
#'   }
#' \item numerator \code{ki_ivus_oct_ved_stenting_lms} has value \emph{ja} if
#' IVUS and/or OCT has been performed.
#' }
#'
#' \code{ki_foreskr_blodfortynnende()}
#' \itemize{
#' \item denominator \code{ki_foreskr_blodfortynnende_dg} (datagrunnlag) is
#' \emph{ja} when all of these conditions are fulfilled:
#' \enumerate{
#'   \item \code{antall_stent_under_opphold} is at least 1
#'   \item \code{Regtype} = "Primær"
#'   \item \code{SkjemaStatusUtskrivelse} is 1 (ferdigstilt)
#'   \item \code{UtskrevetDod}  = "Nei". Which means that "NA", "Ja" or
#'   "Ukjent" are excluded
#'   }
#' \item numerator \code{ki_foreskr_blodfortynnende} has value \emph{ja} if
#' at least two of \code{ASA}, \code{AndrePlatehemmere} or
#'  \code{Antikoagulantia} are prescribed.
#' }
#'
#' \code{ki_foreskr_kolesterolsenkende()}
#' \itemize{
#' \item denominator \code{ki_foreskr_kolesterolsenkende_dg}
#' (datagrunnlag) is \emph{ja} when all of these conditions are fulfilled:
#' \enumerate{
#'   \item \code{antall_stent_under_opphold} is at least 1
#'   \item \code{Regtype} = "Primær"
#'   \item \code{SkjemaStatusUtskrivelse} is 1 (ferdigstilt)
#'   \item \code{UtskrevetDod}  = "Nei". Which means that "NA", "Ja" or
#'   "Ukjent" are excluded
#'   }
#' \item numerator \code{ki_foreskr_kolesterolsenkende} has value \emph{ja}
#' if \code{UtskrStatiner} is "Ja".
#' }
#'
#' \code{ki_nstemi_utredet_innen24t()}
#' \itemize{
#' \item denominator \code{ki_nstemi_utredet_innen24t_dg}
#' (datagrunnlag) is \emph{ja} when all of these conditions are fulfilled:
#' \enumerate{
#'   \item \code{Indikasjone} is "NSTEMI"
#'   \item \code{Regtype} = "Primær"
#'   \item \code{Innkomstarsak} not "Øvrig" (NA allowed)
#'   \item \code{Hastegrad}  = "Akutt" or "Subakutt.
#'   \item \code{OverflyttetFra} neither "Annen  avdeling på sykehuset" nor
#'   "NA".
#'   }
#' \item numerator \code{ki_nstemi_utredet_innen24t} has value \emph{ja}
#' if \code{ventetid_nstemi_timer} is in the interval 0-24hours,
#' value \emph{nei} if  \code{ventetid_nstemi_timer} is in the interval 24hours
#' to 14 days and value \emph{ugyldig/manglende} if time is negative, longer
#' than 14 days or missing. See also function
#'  \link[noric]{legg_til_ventetid_nstemi_timer}.
#' }
#'
#' \code{ki_stemi_pci_innen120min()}
#' \itemize{
#' \item denominator \code{ki_stemi_pci_innen120min_dg}
#' (datagrunnlag) is \emph{ja} when all of these conditions are fulfilled:
#' \enumerate{
#'   \item \code{AvdRESH} is different from 106944 (AHUS Gardermoen does not
#'   treat STEMI patients)
#'   \item \code{Regtype} = "Primær"
#'   \item \code{Indikasjon} = "STEMI"
#'   \item \code{GittTrombolyse} is "Nei" or "NA"
#'   \item \code{Hastegrad}  = "Akutt"
#'   \item \code{HLRForSykehus} different from  "Ja", "Ukjent"
#'   \item \code{ProsedyreType} different from "Angio"
#'   }
#' \item numerator \code{ki_stemi_pci_innen120min} has value \emph{ja}
#' if \code{ventetid_stemi_min} is in the interval 0-120 minutes,
#' value \emph{nei} if  \code{ventetid_stemi_min} is in the interval 120 min
#' to 24h (86400 min) and value \emph{ugyldig/manglende} if time is negative,
#' longer than 24h,  missing or if \code{BeslutningsutlosendeEKG} has value
#' "Prehospitalt" and \code{ventetid_stemi_min} is 0 minutes. See also function
#'  \link[noric]{legg_til_ventetid_stemi_min}.
#' }
#'
#'
#'@param df_ap NORIC's \code{AngioPCIVar}-table. Depending on indicators, must
#' contain some of the variables \code{SkjemaStatusKomplikasjoner},
#' \code{SkjemaStatusUtskrivelse}, \code{Regtype},
#' \code{Indikasjon}, \code{FFR}, \code{IFR},
#' \code{satt_inn_stent_i_LMS}, \code{TidlABC}, \code{IVUS}, \code{OCT},
#' \code{antall_stent_under_opphold}, ...
#'
#'
#' @name utlede_kvalitesindikatorer
#' @aliases
#' ki_ferdigstilt_komplikasjoner
#' ki_trykkmaaling_utfoert
#' ki_ivus_oct_ved_stenting_lms
#' ki_foreskr_blodfortynnende
#' ki_foreskr_kolesterolsenkende
#' ki_nstemi_utredet_innen24t
#' ki_nstemi_utredet_innen72t
#' ki_stemi_pci_innen120min
#'
#' @examples
#'  x <- data.frame(
#'       SkjemaStatusKomplikasjoner = c(-1, 1, 0, NA, NA, NA))
#'  noric::ki_ferdigstilt_komplikasjoner(df_ap = x)
#'
#'  x <- data.frame(
#'       Indikasjon = c(rep("Stabil koronarsykdom", 4), NA, "annet"),
#'       FFR = c("Ja", "Ja", NA, "Ukjent", "Nei", "Ja"),
#'       IFR = c("Ja", "Nei", "Ukjent", NA, NA, NA))
#'  noric::ki_trykkmaaling_utfoert(df_ap = x)
#'
#'  x <- data.frame(
#'       Indikasjon = c(rep("Stabil koronarsykdom", 4), NA, "Annet"),
#'       TidlABC = rep("Nei", 6),
#'       satt_inn_stent_i_LMS = c(rep("ja", 4), NA, "nei"),
#'       IVUS = c("Ja", "Ja", NA, "Ukjent", "Nei", "Ja"),
#'       OCT = c("Ja", "Nei", "Ukjent", NA, NA, NA))
#'  noric::ki_ivus_oct_ved_stenting_lms(df_ap = x)
#'
#'  x <- data.frame(
#'       antall_stent_under_opphold = 1:6,
#'       Regtype = c("Primær", "Primær", "Sekundær", rep("Primær", 3)),
#'       SkjemaStatusUtskrivelse = 1,
#'       UtskrevetDod = c(rep("Nei", 5), "Ja"),
#'       ASA = c("Ja", "Nei", "Ukjent", "Ja", "Ja", "Nei"),
#'       AndrePlatehemmere = c(NA, "Annet", "Clopidogrel (Plavix)",
#'                             "Nei", "Nei", NA),
#'       Antikoagulantia = c(NA, "Annet", "Lixiana", "Marevan",  NA, "Exanta"),
#'       UtskrStatiner = c("Ja", "Ja", NA, "Nei", "Ukjent", "Ja"))
#' noric::ki_foreskr_blodfortynnende(df_ap = x)
#' noric::ki_foreskr_kolesterolsenkende(df_ap = x)
#'
#'  x <- data.frame(
#'       Indikasjon = c("Annet", NA, "NSTEMI", "NSTEMI", "NSTEMI", "NSTEMI"),
#'       Regtype = c("Primær", "Primær", "Sekundær", rep("Primær", 3)),
#'       Innkomstarsak = rep(c("Brystesmerter", "Øvrig", "Dyspne"), 2),
#'       Hastegrad = rep("Subakutt", 6),
#'       OverflyttetFra = c("Annen  avdeling på sykehuset",
#'                          rep("Annet sykehus", 2),
#'                          rep("Omdirigert ambulanse", 3)),
#'       ventetid_nstemi_timer = c(0, 0, 5, 100, NA, -0.1))
#' noric::ki_nstemi_utredet_innen24t(df_ap = x)
#' noric::ki_nstemi_utredet_innen72t(df_ap = x)
#'
#'  x <- data.frame(
#'       AvdRESH = 1:6,
#'       Indikasjon = c("Annet", rep("STEMI",5)),
#'       Regtype = c("Primær", "Primær", "Sekundær", rep("Primær", 3)),
#'       GittTrombolyse = rep("Nei", 6),
#'       Hastegrad = rep("Akutt", 6),
#'       HLRForSykehus = rep("Nei", 6),
#'       ProsedyreType = rep("Angio + PCI", 6),
#'       BeslutningsutlosendeEKG = rep("Prehospitalt", 6),
#'       ventetid_stemi_min = c(-10, 20, 110, 120, 1150, 1480))
#' noric::ki_stemi_pci_innen120min(df_ap = x)


NULL

#' @rdname utlede_kvalitesindikatorer
#' @export
ki_ferdigstilt_komplikasjoner <- function(df_ap) {

  stopifnot("SkjemaStatusKomplikasjoner" %in% names(df_ap))


  df_ap %>%
    dplyr::mutate(

      # Datagrunnlag for indikatoren
      indik_komplik_ferdig_data = dplyr::if_else(
        condition = .data$SkjemaStatusKomplikasjoner %in% c(-1, 0, 1),
        true = "ja",
        false = "nei",
        missing = "nei"),

      # utlede verdi for indikatoren dersom datagrunnlag = "ja"
      indik_komplik_ferdig = dplyr::case_when(

        .data$indik_komplik_ferdig_data == "ja" &
          .data$SkjemaStatusKomplikasjoner == 1 ~ "ja",

        .data$indik_komplik_ferdig_data == "ja" &
          .data$SkjemaStatusKomplikasjoner %in% c(-1, 0) ~ "nei",

        .data$indik_komplik_ferdig_data == "nei" ~ NA_character_,

        FALSE ~ NA_character_))
}


#' @rdname utlede_kvalitesindikatorer
#' @export
ki_trykkmaaling_utfoert <- function(df_ap) {
  stopifnot(all(c("Indikasjon", "FFR", "IFR") %in% names(df_ap)))


  df_ap %>%
    dplyr::mutate(

      # Datagrunnlag for indikatoren
      indik_trykkmaaling_data = dplyr::if_else(
        condition = .data$Indikasjon %in% c("Stabil koronarsykdom"),
        true = "ja",
        false = "nei",
        missing = "nei"),

      # utlede verdi for indikatoren dersom datagrunnlag = "ja"
      # IFR og/eller FFR er utført
      indik_trykkmaaling = dplyr::case_when(

        .data$indik_trykkmaaling_data == "ja" &
          (.data$FFR == "Ja" | .data$IFR == "Ja") ~ "ja",

        .data$indik_trykkmaaling_data == "ja" &
          (.data$FFR != "Ja" & .data$IFR != "Ja") ~ "nei",

        .data$indik_trykkmaaling_data == "ja" &
          is.na(.data$FFR) & .data$IFR != "Ja" ~ "nei",

        .data$indik_trykkmaaling_data == "ja" &
          .data$FFR != "Ja" & is.na(.data$IFR) ~ "nei",

        .data$indik_trykkmaaling_data == "ja" &
          is.na(.data$FFR) & is.na(.data$IFR) ~ "nei",

        .data$indik_trykkmaaling_data == "nei" ~ NA_character_,

        FALSE ~ NA_character_))
}


#' @rdname utlede_kvalitesindikatorer
#' @export
ki_ivus_oct_ved_stenting_lms <- function(df_ap) {
  stopifnot(all(c("Indikasjon", "TidlABC", "IVUS",
                  "OCT", "satt_inn_stent_i_LMS")
                %in% names(df_ap)))


  df_ap %>%
    dplyr::mutate(

      # Datagrunnlag for indikatoren
      #  ~ IKKE tidligere ACB-operert
      #  ~ en av indikasjonene i listen
      #  ~ prosedyre med stenting av venstre hovedstamme (Segment5 = LMS = VH)
      ki_ivus_oct_ved_stenting_lms_dg = dplyr::if_else(
        condition =
          (.data$TidlABC %in% c("Nei", "Ukjent") | is.na(.data$TidlABC)) &
          .data$Indikasjon %in% c("Vitieutredning",
                                  "Uklare brystsmerter",
                                  "Annet",
                                  "Hjertestans uten STEMI",
                                  "Hjertesvikt/kardiomyopati",
                                  "Komplettering av tidligere PCI",
                                  "UAP",
                                  "NSTEMI",
                                  "Stabil koronarsykdom") &
          .data$satt_inn_stent_i_LMS == "ja",

        true = "ja",
        false = "nei",
        missing = "nei"),

      # utlede verdi for indikatoren dersom datagrunnlag = "ja"
      # IVUS og/eller OCT er utført
      ki_ivus_oct_ved_stenting_lms = dplyr::case_when(

        .data$ki_ivus_oct_ved_stenting_lms_dg == "ja" &
          (.data$IVUS == "Ja" | .data$OCT == "Ja") ~ "ja",

        .data$ki_ivus_oct_ved_stenting_lms_dg == "ja" &
          (.data$IVUS != "Ja" & .data$OCT != "Ja") ~ "nei",

        .data$ki_ivus_oct_ved_stenting_lms_dg == "ja" &
          is.na(.data$IVUS) & .data$OCT != "Ja" ~ "nei",

        .data$ki_ivus_oct_ved_stenting_lms_dg == "ja" &
          .data$IVUS != "Ja" & is.na(.data$OCT) ~ "nei",

        .data$ki_ivus_oct_ved_stenting_lms_dg == "ja" &
          is.na(.data$IVUS) & is.na(.data$OCT) ~ "nei",

        .data$ki_ivus_oct_ved_stenting_lms_dg == "nei" ~ NA_character_,

        FALSE ~ NA_character_))
}



#' @rdname utlede_kvalitesindikatorer
#' @export
ki_foreskr_blodfortynnende <- function(df_ap) {
  stopifnot(all(c("antall_stent_under_opphold",
                  "Regtype",
                  "UtskrevetDod",
                  "SkjemaStatusUtskrivelse",
                  "ASA",
                  "AndrePlatehemmere",
                  "Antikoagulantia")
                %in% names(df_ap)))


  df_ap %>%
    dplyr::mutate(

      # Datagrunnlag for indikatoren
      #  ~ Minst en stent satt inn i løpet av oppholdet (primær eller sekundær)
      #  ~ Primærforløp
      #  ~ Ikke utskrevet død, IKKE {Ja, Ukjent, NA}
      #  ~ Ferdigstilt utskrivelsesskjema
      ki_foreskr_blodfortynnende_dg = dplyr::if_else(
        condition =
          (.data$antall_stent_under_opphold > 0 &
             .data$Regtype == "Primær" &
             .data$UtskrevetDod == "Nei" &
             .data$SkjemaStatusUtskrivelse == 1),

        true = "ja",
        false = "nei",
        missing = "nei"),

      # utlede verdi for indikatoren dersom datagrunnlag = "ja"
      # En av de anbefalte kobinasjonene av blodfortynnende medisiner
      ki_foreskr_blodfortynnende = dplyr::case_when(

        # Gyldige kominasjoner:
        .data$ki_foreskr_blodfortynnende_dg == "ja" &
          (.data$ASA == "Ja" &
             !.data$AndrePlatehemmere %in% c("Nei", "Ukjent", NA_character_))
        ~ "ja",

        .data$ki_foreskr_blodfortynnende_dg == "ja" &
          (.data$ASA == "Ja" &
             !.data$Antikoagulantia %in% c("Nei", "Ukjent", NA_character_))
        ~ "ja",

        .data$ki_foreskr_blodfortynnende_dg == "ja" &
          (!.data$AndrePlatehemmere %in% c("Nei", "Ukjent", NA_character_) &
             !.data$Antikoagulantia %in% c("Nei", "Ukjent", NA_character_))
        ~ "ja",

        # Ugyldige kombinasjoner
        .data$ki_foreskr_blodfortynnende_dg == "ja" &
          (.data$ASA == "Ja" &
             .data$AndrePlatehemmere %in% c("Nei", "Ukjent", NA_character_) &
             .data$Antikoagulantia %in% c("Nei", "Ukjent", NA_character_))
        ~ "nei",

        .data$ki_foreskr_blodfortynnende_dg == "ja" &
          (!.data$ASA %in% "Ja" &
             !.data$AndrePlatehemmere %in% c("Nei", "Ukjent", NA_character_) &
             .data$Antikoagulantia %in% c("Nei", "Ukjent", NA_character_))
        ~ "nei",

        .data$ki_foreskr_blodfortynnende_dg == "ja" &
          (!.data$ASA %in% "Ja" &
             .data$AndrePlatehemmere %in% c("Nei", "Ukjent", NA_character_) &
             !.data$Antikoagulantia %in% c("Nei", "Ukjent", NA_character_))
        ~ "nei",

        # Ingen blodfortynnende foreskrevet
        .data$ki_foreskr_blodfortynnende_dg == "ja" &
          (!.data$ASA %in% "Ja" &
             .data$AndrePlatehemmere %in% c("Nei", "Ukjent", NA_character_) &
             .data$Antikoagulantia %in% c("Nei", "Ukjent", NA_character_))
        ~ "nei",

        .data$ki_foreskr_blodfortynnende_dg == "nei" ~ NA_character_,

        FALSE ~ NA_character_))
}



#' @rdname utlede_kvalitesindikatorer
#' @export
ki_foreskr_kolesterolsenkende <- function(df_ap) {
  stopifnot(all(c("antall_stent_under_opphold",
                  "Regtype",
                  "UtskrevetDod",
                  "SkjemaStatusUtskrivelse",
                  "UtskrStatiner")
                %in% names(df_ap)))


  df_ap %>%
    dplyr::mutate(

      # Datagrunnlag for indikatoren
      #  ~ Minst en stent satt inn i løpet av oppholdet
      #  ~ Primærforløp
      #  ~ Ikke utskrevet død, IKKE {Ja, Ukjent, NA}
      #  ~ Ferdigstilt utskrivelsesskjema
      ki_foreskr_kolesterolsenkende_dg = dplyr::if_else(
        condition =
          (.data$antall_stent_under_opphold > 0 &
             .data$Regtype == "Primær" &
             .data$UtskrevetDod == "Nei" &
             .data$SkjemaStatusUtskrivelse == 1),

        true = "ja",
        false = "nei",
        missing = "nei"),

      # utlede verdi for indikatoren dersom datagrunnlag = "ja"
      # og utskrevet Statiner
      ki_foreskr_kolesterolsenkende = dplyr::case_when(

        .data$ki_foreskr_kolesterolsenkende_dg == "ja" &
          .data$UtskrStatiner == "Ja" ~ "ja",

        .data$ki_foreskr_kolesterolsenkende_dg == "ja" &
          (.data$UtskrStatiner == "Nei" |
             is.na(.data$UtskrStatiner) |
             .data$UtskrStatiner == "Ukjent") ~ "nei",


        .data$ki_foreskr_kolesterolsenkende_dg == "nei" ~ NA_character_,

        FALSE ~ NA_character_))
}







#' @rdname utlede_kvalitesindikatorer
#' @export
ki_nstemi_utredet_innen24t <- function(df_ap) {

  stopifnot(all(c("Indikasjon",
                  "Regtype",
                  "Innkomstarsak",
                  "Hastegrad",
                  "OverflyttetFra",
                  "ventetid_nstemi_timer") %in% names(df_ap)))


  df_ap %>%
    dplyr::mutate(

      # Datagrunnlag for indikatoren
      #  ~ Indikasjon NSTEMI
      #  ~ Primærforløp
      #  ~ Innkomstårsak er alt annet enn "Øvrig" (NA er mulig)
      #  ~ Ikke planlagte forløp
      #  ~ OverflyttetFra ulik {NA, "Annen  avdeling på sykehuset"}
      #  MERK 2 mellomrom mellom annen__avdeling
      ki_nstemi_utredet_innen24t_dg = dplyr::if_else(
        condition =
          (.data$Indikasjon == "NSTEMI" &
             .data$Regtype == "Primær" &
             !.data$Innkomstarsak %in% "Øvrig" &  # NA tillates
             !.data$Hastegrad %in% "Planlagt" &
             .data$OverflyttetFra != "Annen  avdeling på sykehuset"), # ikke NA,

        true = "ja",
        false = "nei",
        missing = "nei"),

      # utlede verdi for indikatoren dersom datagrunnlag = "ja"
      # Ugyldig tid (negativ, over 14dg, manglende)
      # Gyldig tid 0 timer til 14 dager.
      # Næyaktig 0.00 timer er ugyldig tid.
      ki_nstemi_utredet_innen24t = dplyr::case_when(

        .data$ki_nstemi_utredet_innen24t_dg == "ja" &
          (!is.na(.data$ventetid_nstemi_timer) &
            .data$ventetid_nstemi_timer > 0.0 &
          .data$ventetid_nstemi_timer <= 24.0) ~ "ja",

        .data$ki_nstemi_utredet_innen24t_dg == "ja" &
          (!is.na(.data$ventetid_nstemi_timer) &
             .data$ventetid_nstemi_timer > 24.0 &
             .data$ventetid_nstemi_timer <= 14 * 24.0) ~ "nei",


        .data$ki_nstemi_utredet_innen24t_dg == "ja" &
          (is.na(.data$ventetid_nstemi_timer) |
             .data$ventetid_nstemi_timer <= 0.0 |
             .data$ventetid_nstemi_timer > 14 * 24) ~ "ugyldig/manglende",



        .data$ki_nstemi_utredet_innen24t_dg == "nei" ~ NA_character_,

        FALSE ~ NA_character_))
}




#' @rdname utlede_kvalitesindikatorer
#' @export
ki_nstemi_utredet_innen72t <- function(df_ap) {

  stopifnot(all(c("Indikasjon",
                  "Regtype",
                  "Innkomstarsak",
                  "Hastegrad",
                  "OverflyttetFra",
                  "ventetid_nstemi_timer") %in% names(df_ap)))


  df_ap %>%
    dplyr::mutate(

      # Datagrunnlag for indikatoren
      #  ~ Indikasjon NSTEMI
      #  ~ Primærforløp
      #  ~ Innkomstårsak er alt annet enn "Øvrig" (NA er mulig)
      #  ~ Ikke planlagte forløp
      #  ~ OverflyttetFra ulik {NA, "Annen  avdeling på sykehuset"}
      #  MERK 2 mellomrom mellom annen__avdeling
      ki_nstemi_utredet_innen72t_dg = dplyr::if_else(
        condition =
          (.data$Indikasjon == "NSTEMI" &
             .data$Regtype == "Primær" &
             !.data$Innkomstarsak %in% "Øvrig" &  # NA tillates
             !.data$Hastegrad %in% "Planlagt" &
             .data$OverflyttetFra != "Annen  avdeling på sykehuset"), # ikke NA,

        true = "ja",
        false = "nei",
        missing = "nei"),

      # utlede verdi for indikatoren dersom datagrunnlag = "ja"
      # gylsig ventetid innen 72t.
      # NB: Dersom ugyldig tid (negativ, over 14dg, manglende) --> NA
      ki_nstemi_utredet_innen72t = dplyr::case_when(

        .data$ki_nstemi_utredet_innen72t_dg == "ja" &
          (!is.na(.data$ventetid_nstemi_timer) &
             .data$ventetid_nstemi_timer > 0.0 &
             .data$ventetid_nstemi_timer <= 72.0) ~ "ja",

        .data$ki_nstemi_utredet_innen72t_dg == "ja" &
          (!is.na(.data$ventetid_nstemi_timer) &
             .data$ventetid_nstemi_timer > 72.0 &
             .data$ventetid_nstemi_timer <= 14 * 24) ~ "nei",


        .data$ki_nstemi_utredet_innen72t_dg == "ja" &
          (is.na(.data$ventetid_nstemi_timer) |
             .data$ventetid_nstemi_timer <= 0.0 |
             .data$ventetid_nstemi_timer > 14 * 24) ~ "ugyldig/manglende",



        .data$ki_nstemi_utredet_innen72t_dg == "nei" ~ NA_character_,

        FALSE ~ NA_character_))
}








#' @rdname utlede_kvalitesindikatorer
#' @export
ki_stemi_pci_innen120min <- function(df_ap) {

  stopifnot(all(c("AvdRESH",
                  "Indikasjon",
                  "Regtype",
                  "GittTrombolyse",
                  "Hastegrad",
                  "HLRForSykehus",
                  "ProsedyreType",
                  "BeslutningsutlosendeEKG",
                  "ventetid_stemi_min") %in% names(df_ap)))


  df_ap %>%
    dplyr::mutate(

      # Datagrunnlag for indikatoren
      #  ~ Ikke AHUS GARDERMOEN
      #  ~ Indikasjon STEMI
      #  ~ Primærforløp
      #  ~ Ikke gitt trombolyse
      #  ~ Kun akutte forløp
      #  ~ Ikke hjerte-lungeredning før sykehus
      #  ~ Ikke prosedyretype Angio
      ki_stemi_pci_innen120min_dg = dplyr::if_else(
        condition =
          (.data$AvdRESH != 106944 &
             .data$Indikasjon == "STEMI" &
             .data$Regtype == "Primær" &
             .data$GittTrombolyse %in% c("Nei", NA) &  # NA tillates
             .data$Hastegrad %in% "Akutt" &
             !.data$HLRForSykehus %in% c("Ja", "Ukjent") &
             .data$ProsedyreType != "Angio"),


        true = "ja",
        false = "nei",
        missing = "nei"),

      # utlede verdi for indikatoren dersom datagrunnlag = "ja"
      # gylsig ventetid innen 24t.
      # BeslutningsutlosendeEKG ikke gitt prehospitalt og 0 minutter ventetid
      # NB: Dersom ugyldig tid (negativ, over 24t, manglende) --> NA
      ki_stemi_pci_innen120min = dplyr::case_when(

        .data$ki_stemi_pci_innen120min_dg == "ja" &
          .data$BeslutningsutlosendeEKG %in% "Prehospitalt" &
          .data$ventetid_stemi_min == 0 ~ "ugyldig/manglende",

        .data$ki_stemi_pci_innen120min_dg == "ja" &
          (!is.na(.data$ventetid_stemi_min) &
             .data$ventetid_stemi_min >= 0 &
             .data$ventetid_stemi_min <= 120) ~ "ja",

        .data$ki_stemi_pci_innen120min_dg == "ja" &
          (!is.na(.data$ventetid_stemi_min) &
             .data$ventetid_stemi_min > 120 &
             .data$ventetid_stemi_min <= 24*60) ~ "nei",

        .data$ki_stemi_pci_innen120min_dg == "ja" &
          (is.na(.data$ventetid_stemi_min) |
             .data$ventetid_stemi_min < 0 |
             .data$ventetid_stemi_min > 24*60) ~ "ugyldig/manglende",

        .data$ki_stemi_pci_innen120min_dg == "nei" ~ NA_character_,

        FALSE ~ NA_character_))
}










