#' Make staging data for KI-report
#'
#'Make RData dataset needed for national KI-report
#' @return saved RData-set 
#' @export
#'
#' @examples
makeStagingDataKi <- function(registryName) {
  
  # Sjekke om dette kan gjøres i funksjonen etterhvert, 
  # som periode_data i indikator-rapporten!
  forste_dato <- as.Date("2018-01-01", format = "%Y-%m-%d")
  siste_dato <- as.Date(Sys.time()) - 1
  
  
  
  
  # HENTE DATA:
  sS_nasjonalt <- noric::getPrepSsData(
    registryName = registryName, 
    fromDate  = forste_dato,  
    toDate = siste_dato, 
    singleRow = FALSE) 
  
  
  # Hardkodet 2018- dags dato pga figur 4,5 og 6
  aP_nasjonalt <- noric::getPrepApData(
    registryName = registryName, 
    fromDate  = forste_dato,  
    toDate = siste_dato, 
    singleRow = FALSE)
  
  aK_nasjonalt <- noric::getPrepAkData(
    registryName = registryName, 
    fromDate  = forste_dato,  
    toDate = siste_dato, 
    singleRow = FALSE)
  
  anD_nasjonalt <- noric::getPrepAnDData(
    registryName = registryName, 
    fromDate  = forste_dato,  
    toDate = siste_dato, 
    singleRow = FALSE)
  
  
  # BEARBEIDE DATA: 
  sS_nasjonalt %<>% 
    dplyr::select(.data$ProsedyreDato,
                  .data$StentType, 
                  .data$Segment, 
                  .data$Graft,
                  .data$ForlopsID,
                  .data$Sykehusnavn, 
                  .data$AvdRESH, 
                  .data$aar, 
                  .data$maaned, 
                  .data$kvartal)
  
  
  aP_nasjonalt %<>% 
    dplyr::select(
      .data$AvdRESH,
      .data$Sykehusnavn,
      .data$Regtype, 
      .data$PrimaerForlopsID,
      .data$ProsedyreDato,
      .data$ProsedyreTid,
      .data$ProsedyreType,
      .data$OverflyttetFra, 
      .data$AnkomstPCIDato,
      .data$AnkomstPCITid, 
      .data$InnleggelseHenvisendeSykehusDato,
      .data$InnleggelseHenvisendeSykehusTid,
      .data$Innkomstarsak,
      .data$Indikasjon, 
      .data$Hastegrad,
      .data$BesUtlEKGDato, 
      .data$BesUtlEKGTid,
      .data$BeslutningsutlosendeEKG,
      .data$GittTrombolyse, 
      .data$HLRForSykehus,
      .data$KobletForlopsID,
      .data$ForlopsType2, 
      .data$IFR, 
      .data$FFR, 
      .data$IVUS, 
      .data$OCT, 
      .data$ForlopsID, 
      .data$ASA, 
      .data$AndrePlatehemmere,
      .data$AndrePlatehemmere,
      .data$Antikoagulantia,
      .data$UtskrStatiner, 
      .data$TidlABC, 
      .data$UtskrevetDod, 
      .data$SkjemaStatusStart, 
      .data$SkjemastatusHovedskjema, 
      .data$SkjemaStatusUtskrivelse, 
      .data$SkjemaStatusKomplikasjoner
    ) %>% 
    
    noric::utlede_OppholdsID(.) %>% 
    
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
    
    noric::legg_til_antall_stent(df_ap = ., df_ss = sS_nasjonalt) %>% 
    noric::legg_til_antall_stent_opphold(df_ap = .) %>% 
    noric::satt_inn_stent_i_lms(df_ap = ., df_ss = sS_nasjonalt) %>% 
    
    #  Legge til utledete variabler fra annen Diagnostikk. Hjelpevariabler for
    # trykkmåling. Disse fjernes før tabellen legges i utforsker
    noric::legg_til_trykkmaalinger(df_ap = .,
                                   df_ad = anD_nasjonalt) %>% 
    
    # LEgg til hjelpevariabler for ventetider
    noric::legg_til_ventetid_nstemi_timer(.) %>% 
    noric::legg_til_ventetid_stemi_min(.) %>% 
    
    # Legge til kvalitetsindikatorene:
    noric::ki_ferdigstilt_komplikasjoner(df_ap = .) %>% 
    noric::ki_trykkmaaling_utfoert(df_ap = .) %>% 
    noric::ki_ivus_oct_ved_stenting_lms(df_ap = .) %>% 
    noric::ki_foreskr_blodfortynnende(df_ap = .) %>% 
    noric::ki_foreskr_kolesterolsenkende(df_ap = .) %>%
    noric::ki_nstemi_utredet_innen24t() %>% 
    noric::ki_nstemi_utredet_innen72t() %>% 
    noric::ki_stemi_pci_innen120min() %>% 
    
    dplyr::mutate(
      maaned = as.factor(format(x = .data$ProsedyreDato, format = "%Y-%m")), 
      aar = lubridate::year(.data$ProsedyreDato), 
      kvartal = paste0(lubridate::year(.data$ProsedyreDato), 
                       " Q", 
                       lubridate::quarter(.data$ProsedyreDato, 
                                          with_year = FALSE))) %>% 
    dplyr::mutate(admissionType = dplyr::case_when(
      .data$OverflyttetFra %in% "Annet sykehus" ~ "Overflyttet", 
      .data$OverflyttetFra %in% c("Nei, direkte inn til dette sykehus", 
                                  "Omdirigert ambulanse") ~ "Direkte",
      TRUE ~ NA_character_
    ))
  
  
  
  
  aK_nasjonalt %<>% 
    dplyr::select(
      .data$ProsedyreDato,
      .data$AvdKompPacemaker, 
      .data$LabKompDod, 
      .data$TypeKlaffeprotese,
      .data$Sykehusnavn, 
      .data$AvdRESH, 
      .data$ForlopsID, 
      .data$Pacemaker,
      .data$SkjemaStatusHovedskjema)
  
  aK_nasjonalt %<>%
    dplyr::mutate(
      kvartal = paste0(
        lubridate::year(.data$ProsedyreDato),
        " Q",
        lubridate::quarter(.data$ProsedyreDato, with_year = FALSE))) %>% 
    noric::ki_ak_pacemakerbehov(df_ak = .)
  
  # VELGE UT VARIABLER SOM SKAL BEHOLDES:
  # Her kan vi begrense antall variabler og rader.., 
  # velge et navn på datasettes som inneholder dato kanskje.
  
  
  # LAGRE STAGING DATA
  rapbase::saveStagingData(registryName = registryName,
                           dataName = "staging_nasjonal_ki", 
                           data = list(aK_nasjonalt = aK_nasjonalt, 
                                       aP_nasjonalt = aP_nasjonalt))
}