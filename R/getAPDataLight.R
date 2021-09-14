#' getAPDataLight provides local reg data from AngioPCIVar + useful generated
#' variables
#'
#' @param registryName String providing the registry name
#' @param singleRow Logical defining if only one row is to be returned. A
#' relevant usecase will be when only description is needed. By default set to
#' FALSE
#' @param ... Optional arguments to be passed to the function
#' @importFrom dplyr filter mutate mutate_all select left_join
#'
#' @return Data frame representing a simplified version of angio PCI-data
#' @export
#'

getAPDataLight <- function(registryName, singleRow = FALSE, ...) {


  if (singleRow) {
    msg <- "Query metadata for AngioPCIlight pivot"
    if ("session" %in% names(list(...))) {
      rapbase::repLogger(session = list(...)[["session"]], msg = msg)
    }

    # Load only first row for all tables
    ap_light <- rapbase::loadRegData(
      registryName,
      query = "SELECT * FROM AngioPCIVar LIMIT 1;")

    fO <- rapbase::loadRegData(
      registryName,
      query = "SELECT * FROM ForlopsOversikt LIMIT 1;")

    sS <- rapbase::loadRegData(
      registryName,
      query = "SELECT * FROM SegmentStent LIMIT 1;")

    aD <- rapbase::loadRegData(
      registryName,
      query = "SELECT * FROM AnnenDiagnostikkVar LIMIT 1;")


  } else {
    msg <- "Query data for AngioPCI light pivot"
    if ("session" %in% names(list(...))) {
      rapbase::repLogger(session = list(...)[["session"]], msg = msg)
    }

    #  Load complete tables from 3 last years until today
    latest_entry <- noric::getLatestEntry(registryName = registryName)
    last_3_years <- as.Date(
      paste0(as.numeric(lubridate::year(latest_entry))-3, "-01-01"),
      format = "%Y-%m-%d")

    ap_light <- rapbase::loadRegData(
      registryName,
      query = paste0("SELECT * FROM AngioPCIVar WHERE ProsedyreDato >= '",
                     last_3_years,
                     "' ;"))

    fO <- rapbase::loadRegData(
      registryName,
      query = paste0("SELECT * FROM ForlopsOversikt WHERE HovedDato >= '",
                     last_3_years,
                     "' ;"))

    sS <- rapbase::loadRegData(
      registryName,
      query = paste0("SELECT * FROM SegmentStent WHERE ProsedyreDato >= '",
                     last_3_years,
                     "' ;"))

    aD <- rapbase::loadRegData(
      registryName,
      query = paste0(
        "SELECT * FROM AnnenDiagnostikkVar WHERE ProsedyreDato >= '",
        last_3_years,
        "' ;"))
  }




  # Legger til variabler fra fO til aP
  ap_light %<>%  dplyr::left_join(
    x = .,
    y = fO %>%
      dplyr::select(
        # Nøkler:
        .data$AvdRESH,
        .data$PasientID,
        .data$ForlopsID,

        # Variablene som legges til:
        .data$Kommune,
        .data$KommuneNr,
        .data$Fylke,
        .data$Fylkenr,
        .data$PasientAlder,
        .data$KobletForlopsID),
    by = c("AvdRESH", "PasientID", "ForlopsID"))

  # Tar bort forløp fra før sykehusene ble offisielt med i NORIC
  # (potensielle "tøyseregistreringer")
  ap_light %<>% noric::fjerne_tulleregistreringer(df = .,
                                                  var = .data$ProsedyreDato)

  # Legg til oppholdsID (samme ID for alle prosedyrer tilknyttet samme sykehus-
  # opphold)
  ap_light %<>% noric::utlede_OppholdsID(df =.)

  # Gjor datoer om til dato-objekt:
  ap_light %<>%
    dplyr::mutate_at(
      vars(ends_with("dato", ignore.case = TRUE)),
      list(lubridate::ymd))

  # Utledete tidsvariabler (aar, maaned, uke osv):
  ap_light %<>% noric::legg_til_tidsvariabler(df = .,
                                              var = .data$ProsedyreDato)


  # Endre Sykehusnavn til kortere versjoner:
  ap_light %<>% noric::fikse_sykehusnavn(df = .)


  # Utlede variabler for ferdigstilt eller ikke,
  # Fjerne {-1,0,1}-variablene fra tabellen (forenkle!!)
  ap_light %<>%
    noric::utlede_ferdigstilt(df = .,
                              var = .data$SkjemaStatusStart,
                              suffix = "StartSkjema") %>%

    noric::utlede_ferdigstilt(df = .,
                              var = .data$SkjemastatusHovedskjema,
                              suffix = "HovedSkjema") %>%

    noric::utlede_ferdigstilt(df = .,
                              var = .data$SkjemaStatusUtskrivelse,
                              suffix = "UtskrSkjema") %>%

    noric::utlede_ferdigstilt(df = .,
                              var = .data$SkjemaStatusKomplikasjoner,
                              suffix = "KomplikSkjema")

  # Utlede aldersklasser
  ap_light %<>% noric::utlede_aldersklasse(df = .,
                                           var = .data$PasientAlder)

  # Legger til utledete variabler fra segment Stent til ap_light
  ap_light %<>% noric::legg_til_antall_stent(df_ap = .,
                                             df_ss = sS)
  ap_light %<>% noric::legg_til_antall_stent_per_opphold(df_ap = .)
  ap_light %<>% noric::satt_inn_stent_i_lms(df_ap = .,
                                            df_ss = sS)
  ap_light %<>% noric::legg_til_pci_per_kar(df_ap = .,
                                            df_ss = sS)


  # Legger til utledete varibler fra Annen Diagnostikk-tabellen
  ap_light %<>%
    noric::legg_til_trykk_bilde_per_kar(df_ap = .,
                                        df_ad = aD,
                                        metodeType = "FFR") %>%
    noric::legg_til_trykk_bilde_per_kar(df_ap = .,
                                        df_ad = aD,
                                        metodeType = "iFR") %>%
    noric::legg_til_trykk_bilde_per_kar(df_ap = .,
                                        df_ad = aD,
                                        metodeType = "IVUS") %>%
    noric::legg_til_trykk_bilde_per_kar(df_ap = .,
                                        df_ad = aD,
                                        metodeType = "OCT")


  # Legge til kvalitetsindikatorene:
  ap_light %<>% noric::ki_ferdigstilt_komplikasjoner(df_ap = .)
  ap_light %<>% noric::ki_trykkmaaling_utfoert(df_ap = .)
  ap_light %<>% noric::ki_ivus_oct_ved_stenting_lms(df_ap = .)
  ap_light %<>% noric::ki_foreskr_blodfortynnende(df_ap = .)
  ap_light %<>% noric::ki_foreskr_kolesterolsenkende(df_ap = .)

  ap_light %<>%
    noric::legg_til_ventetid_nstemi_timer(df_ap = .) %>%
    noric::ki_nstemi_utredet_innen24t(df_ap = .) %>%
    noric::ki_nstemi_utredet_innen72t(df_ap = .) %>%
    dplyr::select(-.data$ventetid_nstemi_timer)

  ap_light %<>%
    noric::legg_til_ventetid_stemi_min(df_ap = .) %>%
    noric::ki_stemi_pci_innen120min(df_ap = .) %>%
    dplyr::select(-.data$ventetid_stemi_min)


  # Gjøre kategoriske variabler om til factor:
  # (ikke fullstendig, må legge til mer etter hvert)
  ap_light %<>%
    dplyr::mutate(
      Hastegrad = factor(.data$Hastegrad,
                         levels = c("Akutt",
                                    "Subakutt",
                                    "Planlagt"),
                         ordered = TRUE))


  # # Bare en variabel for symptomdebutDato - og tid.  har sjekket de er like.
  # #  Dersom en av dem er NA - brukes den andre, og omvendt
  # ap_light %<>%
  #   noric::slaa_sammen_variabler(df = .,
  #                                var1 = .data$SymptomDato,
  #                                var2 = .data$SymptomdebutDato,
  #                                var_name = "SymptomDebutDato",
  #                                slette_gamle = TRUE) %>%
  #   noric::slaa_sammen_variabler(df = .,
  #                                var1 = .data$SymptomTid,
  #                                var2 = .data$SymptomdebutTid,
  #                                var_name = "SymptomDebutTid",
  #                                slette_gamle = TRUE)

  # TO DO:
  # BesUtlEKGDato  og BeslEKGDato. Beholde berre ein variabel
  #  + (BesUtlEKGTud, TidUkjent)
  # Dei er ikkje identiske for sekundærforløp med STEMI, NSTEMI,
  # hjertestans osv
  # Bør vi ha verdi eller ikkje for desse forløpa ??

  #  ap_light %<>% dplyr::select(-.data$BesUtlEKGDato,
  # -.data$BesUtlEKGTid,
  # -.data$BesUtlEKGTidUkjent )


  # Fjerne noen variabler
  # ap_light %<>%
  #   dplyr::select(- tidyselect::contains("Ukjent"),
  #                 - .data$FodselsDato,
  #                 - .data$PasientRegDato,
  #                 - .data$Studie,
  #                 - tidyselect::contains("SEGMENT"),
  #                 - .data$SkjemaStatusStart,
  #                 - .data$SkjemastatusHovedskjema,
  #                 - .data$SkjemaStatusUtskrivelse,
  #                 - .data$SkjemaStatusKomplikasjoner)
  #
  #

  ap_light
}
