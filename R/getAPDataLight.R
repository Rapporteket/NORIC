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


  aP_light <- rapbase::loadRegData(registryName,
                                   query = "SELECT * FROM AngioPCIVar")
  fO <- rapbase::loadRegData(registryName,
                             query = "SELECT * FROM ForlopsOversikt")
  sS <- rapbase::loadRegData(registryName,
                             query = "SELECT * FROM SegmentStent")

  aD <- rapbase::loadRegData(registryName,
                             query = "SELECT * FROM AnnenDiag")

  # Legger til variabler fra fO til aP
  aP_light %<>%  dplyr::left_join(
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
    by = c("AvdRESH","PasientID","ForlopsID"))

  # Tar bort forløp fra før sykehusene ble offisielt med i NORIC
  # (potensielle "tøyseregistreringer")
  aP_light %<>% noric::fjerne_tulleregistreringer(df = .,
                                                  var = .data$ProsedyreData)

  # Gjor datoer om til dato-objekt:
  aP_light %<>%
    dplyr::mutate_at(
      vars(ends_with("dato", ignore.case = TRUE)),
      list(ymd))

  # Utledete tidsvariabler (aar, maaned, uke osv):
  aP_light %<>% noric::legg_til_tidsvariabler(df = .,
                                              var = .data$ProsedyreDato)


  # Endre Sykehusnavn til kortere versjoner:
  aP_light %<>% noric::fikse_sykehusnavn(df = .)


  # Utlede variabler for ferdigstilt eller ikke,
  # Fjerne {-1,0,1}-variablene fra tabellen (forenkle!!)
  aP_light %<>%
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
                              suffix = "KomplikSkjema") %>%
    dplyr::select(- .data$SkjemaStatusStart,
                  - .data$SkjemastatusHovedskjema,
                  - .data$SkjemaStatusUtskrivelse
                  - .data$SkjemaStatusKomplikasjoner)

  # Utlede aldersklasser
  aP_light %<>%
    utlede_aldersklasse(df = .,
                        var = .data$PasientAlder)

  # Legger til utledete variabler fra segment Stent til aP_light
  aP_light %<>% noric::legg_til_antall_stent(df_ap = .,
                                             df_ss = sS)

  aP_light %<>% noric::legg_til_pci_per_kar(df_ap = .,
                                            df_ss = sS)


  # Legger til utledete varibler fra Annen Diagnostikk-tabellen
  aP_light %<>%
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
  aP_light %<>% noric::ki_ferdigstilt_komplikasjoner()
  aP_light %<>% noric::ki_trykkmaaling_utfoert()


  # Gjøre kategoriske variabler om til factor:
  # (ikke fullstendig, må legge til mer etter hvert)
  aP_light %<>%
    dplyr::mutate(
      Hastegrad = factor(.data$Hastegrad,
                         levels = c("Akutt",
                                    "Subakutt",
                                    "Planlagt"),
                         ordered = TRUE))

  # Bare en variabel for symptomdebutDato - og tid.  har sjekket de er like.
  #  Dersom en av dem er NA - brukes den andre, og omvendt
  aP_light %<>%
    noric::slaa_sammen_variabler(df = .,
                                 var1 = SymptomDato,
                                 var2 = SymptomDebutDato,
                                 var_name = "SymptomdebutDato",
                                 slette_gamle = TRUE) %>%
    noric::slaa_sammen_variabler(df = .,
                                 var1 = SymptomTid,
                                 var2 = SymptomDebutTid,
                                 var_name = "SymptomdebutTid",
                                 slette_gamle = TRUE)

  # TO DO:
  # BesUtlEKGDato  og BeslEKGDato. Beholde berre ein variabel
  #  + (BesUtlEKGTud, TidUkjent)
  # Dei er ikkje identiske for sekundærforløp med STEMI, NSTEMI, hjertestans osv
  # Bør vi ha verdi eller ikkje for desse forløpa ??

  #  aP_light %<>% dplyr::select(-.data$BesUtlEKGDato,
                                 # -.data$BesUtlEKGTid,
                                 # -.data$BesUtlEKGTidUkjent )


  # Fjerne noen variabler
  aP_light %<>%
    dplyr::select(- contains("Ukjent")
                  - .data$FodselsDato,
                  - .data$PasientRegDato,
                  - .data$Studie,
                  - contains("SEGMENT"))


  aP_light
}
