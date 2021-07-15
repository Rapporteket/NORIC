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
                                   query = "SELECT * FROM AngioPCIVar",
                                   dbType)

  fO <- rapbase::loadRegData(registryName,
                             query = "SELECT * FROM ForlopsOversikt")
  sS <- rapbase::loadRegData(registryName,
                             query = "SELECT * FROM SegmentStent")

  aD <- rapbase::loadRegData(registryName,
                             query = "SELECT * FROM AnnenDiag")

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
    by = c("AvdRESH","PasientID","ForlopsID"))

  # Tar bort forløp fra før sykehusene ble offisielt med i NORIC
  # (potensielle "tøyseregistreringer")
  ap_light %<>% noric::fjerne_tulleregistreringer(df = .,
                                                  var = .data$ProsedyreData)

  # Gjor datoer om til dato-objekt:
  ap_light %<>%
    dplyr::mutate_at(
      vars(ends_with("dato", ignore.case = TRUE)),
      list(ymd))

  # Utledete tidsvariabler (aar, maaned, uke osv):
  ap_light %<>% noric::legg_til_tidsvariabler(df = .,
                                              var = .data$ProsedyreDato)


  # Endre Sykehusnavn til kortere versjoner:
  ap_light %<>% noric::fikse_sykehusnavn(df = .)


  # Utlede variabler for ferdigstilt eller ikke
  ap_light %<>%
    noric::utlede_ferdigstilt(df = .,
                              var = SkjemaStatusStart,
                              suffix = "StartSkjema") %>%

    noric::utlede_ferdigstilt(df = .,
                              var = SkjemastatusHovedskjema,
                              suffix = "HovedSkjema") %>%

    noric::utlede_ferdigstilt(df = .,
                              var = SkjemaStatusUtskrivelse,
                              suffix = "UtskrSkjema") %>%

    noric::utlede_ferdigstilt(df = .,
                              var = SkjemaStatusKomplikasjoner,
                              suffix = "KomplikSkjema")

  # Utlede aldersklasser
  ap_light %<>%
    utlede_aldersklasse(df = .,
                        var = .data$PasientAlder) %>%
    dplyr::relocate(.data$aldersklasse,
                    .after = .data$PasientAlder)

  # Legger til utledete variabler fra segment Stent til ap_light
  ap_light %<>% noric::legg_til_antall_stent(df_ap = .,
                                             df_ss = sS)

  ap_light %<>% noric::legg_til_pci_per_kar(df_ap = .,
                                            df_ss = sS)


  # Legger til utledete varibler fra Annen Diagnostikk-tabellen
  ap_light %<>% noric::legg_til_ffr_per_kar(df_ap = .,
                                            df_ad = aD)




  # Gjøre kategoriske variabler om til factor:
  # (ikke fullstendig, må legge til mer etter hvert)
  ap_light %<>%
    dplyr::mutate(
      Hastegrad = factor(.data$Hastegrad,
                         levels = c("Akutt",
                                    "Subakutt",
                                    "Planlagt"),
                         ordered = TRUE))

  return(ap_light)
}
