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

  . <- ""

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

  # Legger til utledete variabler fra segment Stent til ap_light,
  # Noen er hjelpevariabler som brukes i KI-funksjonene. Disse fjernes
  # før de legges i utforsker.
  ap_light %<>% noric::legg_til_antall_stent(df_ap = .,
                                             df_ss = sS)
  ap_light %<>% noric::legg_til_antall_stent_per_opphold(df_ap = .)
  ap_light %<>% noric::satt_inn_stent_i_lms(df_ap = .,
                                            df_ss = sS)


  # Legge til kvalitetsindikatorene:
  ap_light %<>% noric::ki_ferdigstilt_komplikasjoner(df_ap = .)
  ap_light %<>% noric::ki_trykkmaaling_utfoert(df_ap = .)
  ap_light %<>% noric::ki_ivus_oct_ved_stenting_lms(df_ap = .)
  ap_light %<>% noric::ki_foreskr_blodfortynnende(df_ap = .)
  ap_light %<>% noric::ki_foreskr_kolesterolsenkende(df_ap = .)

  ap_light %<>%
    noric::legg_til_ventetid_nstemi_timer(df_ap = .) %>%
    noric::ki_nstemi_utredet_innen24t(df_ap = .) %>%
    noric::ki_nstemi_utredet_innen72t(df_ap = .)
  ap_light %<>%
    noric::legg_til_ventetid_stemi_min(df_ap = .) %>%
    noric::ki_stemi_pci_innen120min(df_ap = .)



  # Legg til liggedogn
  ap_light %<>% noric::legg_til_liggedogn(df_ap = .)


  # Fjerne noen variabler.
  #  Se variablliste fra NORIC
  ap_light %<>%
    dplyr::select(
      # Foretrekker de utledete "ferdigstilt.. " variablene:
      - .data$SkjemaStatusStart,
      - .data$SkjemastatusHovedskjema,
      - .data$SkjemaStatusUtskrivelse,
      - .data$SkjemaStatusKomplikasjoner,

      # Overflødig, fordi tilhørende kont. verdi er NA:
      - tidyselect::contains("Ukjent"),

      # Ikke i bruk
      - .data$PasientRegDato,
      - .data$Studie,

      # Dobbelt opp av disse, fjerne minst komplette/feil (sept 2021):
      -.data$BesUtlEKGDato,
      -.data$BesUtlEKGTid,
      -.data$KillipKlasseAnkomst,
      -.data$KardiogentSjokk,
      -.data$Kreatinin,

      # Fjerne alle init-medikamenter:
      -.data$InitASA,
      -.data$InitAntikoagulantia,
      -.data$InitAndrePlatehemmere,
      -.data$InitStatiner,
      -.data$InitNSAID,
      -.data$InitACEHemmere,
      -.data$InitA2Blokkere,
      -.data$InitBetaBlokkere,
      -.data$InitCaHemmere,
      -.data$InitDiabetesPrOral,
      -.data$InitDigitalis,
      -.data$InitDiuretika,
      -.data$InitAldosteronantagonist,
      -.data$InitOvrigLipid,
      -.data$InitNitroglycerin,

      # Fjerne alle init blodprøver
      - .data$Infarktmarkoer,
      - .data$InfarktMarkoerMax,
      - .data$Kolesterol,
      - .data$Triglycerider,
      - .data$HDL,
      - .data$MaaltLDL,
      - .data$SGlukose,
      - .data$HbA1c,
      - .data$Kreatinin,
      - .data$CRP,
      - .data$Hemoglobin,

      # # Fjerne medikamenter Før
      # - .data$AntitrombotiskFor,
      # - .data$TrombolyseFor,
      # - .data$ASAFor,
      # - .data$ClopidogrelFor,
      # - .data$PrasugrelFor,
      # - .data$TicagrelorFor,
      # - .data$HeparinFor,
      # - .data$DalteparinFor,
      # - .data$EnoxaparinFor,
      # - .data$AnnetLavmolHeparinFor,
      # - .data$BivalirudinFor,
      # - .data$FondaparinuxFor,
      # - .data$AbciximabFor,
      # - .data$EptifibatidFor,
      # - .data$TirofibanFor,
      # - .data$WarfarinFor,
      # - .data$DabigatranFor,
      # - .data$ApiksabanFor,
      # - .data$RivaroksabanFor,
      # - .data$EdoksabanFor,
      # - .data$KangrelorFor,
      # - .data$AnnetAntitrombotiskFor
      #
      # # Fjerne medikamenter under
      # - .data$AntitrombotiskUnder,
      # - .data$TrombolyseUnder,
      # - .data$ASAUnder,
      # - .data$ClopidogrelUnder,
      # - .data$PrasugrelUnder,
      # - .data$TicagrelorUnder,
      # - .data$HeparinUnder,
      # - .data$DalteparinUnder,
      # - .data$EnoxaparinUnder,
      # - .data$AnnetLavmolHeparinUnder,
      # - .data$BivalirudinUnder,
      # - .data$FondaparinuxUnder,
      # - .data$AbciximabUnder,
      # - .data$EptifibatidUnder,
      # - .data$TirofibanUnder,
      # - .data$WarfarinUnder,
      # - .data$KangrelorUnder,
      # - .data$AnnetAntitrombotiskUnder,

      # Fjerne komplikasjoner
      - tidyselect::contains("AvdKomp"),
      - tidyselect::contains("LabKomp")


      # Mediakmenter ved utskrivelse:
      - .data$NSAID,
      - .data$ACEHemmere,
      - .data$A2Blokkere,
      - .data$Betablokkere,
      - .data$CaBlokkere,
      - .data$DiabetesBehandlingInsulin,
      - .data$DiabetesBehandlingPerOral,
      - .data$Digitalis,
      - .data$Diuretika,
      - .data$Aldosteronantagonister,
      - .data$NitroglycerinLangtid,

      # Andre variabler utskrivelse
      - .data$InfarktType,
      - .data$InfarktSubklasse,
      - .data$UtskrDiagnoser
)

  # Fjerne utledete hjelpevariabler
  ap_light %<>%
    dplyr::select(- .data$antall_stent_under_opphold,
                  - .data$satt_inn_stent_i_LMS)

  # Gjøre kategoriske variabler om til factor:
  ap_light %<>%
    dplyr::mutate(
      Hastegrad = factor(.data$Hastegrad,
                         levels = c("Akutt",
                                    "Subakutt",
                                    "Planlagt"),
                         ordered = TRUE))

  ap_light
}
