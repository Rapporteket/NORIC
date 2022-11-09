#' Make staging data for KI-report
#'
#'Make RData dataset needed for national KI-report
#' @return saved RData-set 
#' @export
#'
#' @examples
makeStagingDataKi <- function(registryName, rendered_by_shiny = FALSE) {
  
  
  if(rendered_by_shiny){
    shiny::setProgress(0.05)
  }
  
  # DATAGRUNNLAG: PERIODE FOR SQL SPØRRING
  
  # NYESTE DATO: 
  # Finne nyeste prosedyredato (=nyeste registrering). Vi ønsker ikke at 
  # forhåndsregisrerte planlagte forløp kommer med i rapporten. Derfor brukes
  # gårsdagens dato som referanse, ingen forløp etter denne kommer med .  
  # Vi vil dermed også kunne se dersom ingen nye registreringer gjøres eller om
  # overføringer har stoppet opp
  
  # ELDSTE DATO: 
  # generelt :Januar fra i fjor (hele foregående år skal vise i rapporten)
  # AK: Bruker 5 siste kvartal. Kan gå lenger tilbake enn fjoråret, dersom i 
  #     starten av året. 
  # SS: Brukes til "antall_stent_under_opphold" (foreskriving av medikamenter) +          
  # "stenting_av_venstre_hovedstamme" (IVUS/OCT - Per kvartal!)
  
  
  periode_data <- data.frame(
    # Nyeste registrering eller gårsdagen
    siste_dato = min((as.Date(Sys.time()) - 1), 
                     noric::getLatestEntry(registryName = registryName))) %>% 
    
    dplyr::mutate(
      # Inneværende år: 
      nyesteRegYear = as.numeric(format(.data$siste_dato, format = "%Y")),
      
      # Fjoråret
      sisteHeleYear = .data$nyesteRegYear - 1, 
      
      # Første dato for SQL -spørring : 01. januar fjoråret
      forste_dato  = as.Date(paste0(sisteHeleYear, "-01-01"),
                             format = "%Y-%m-%d"), 
      
      # Aortaklaff + Segment stent (indikator per kvartal): 
      forste_dato_ak_ss = as.Date(paste0(sisteHeleYear -1, "-01-01"),
                                  format = "%Y-%m-%d")
    )
  
  
  if(rendered_by_shiny){
    shiny::setProgress(0.10) 
  }
  
  # HENTE DATA:
  sS_nasjonalt <- noric::getPrepSsData(
    registryName = registryName, 
    fromDate  = periode_data$forste_dato_ak_ss,  
    toDate = periode_data$siste_dato, 
    singleRow = FALSE) 
  
  if(rendered_by_shiny){
    shiny::setProgress(0.20)
  }
  
  # Hardkodet 2018- dags dato pga figur 4,5 og 6
  aP_nasjonalt <- noric::getPrepApData(
    registryName = registryName, 
    fromDate  = as.Date("2018-01-01", format = "%Y-%m-%d"),  
    toDate = periode_data$siste_dato, 
    singleRow = FALSE)
  
  if(rendered_by_shiny){
    shiny::setProgress(0.50) 
  }
  
  aK_nasjonalt <- noric::getPrepAkData(
    registryName = registryName, 
    fromDate  = periode_data$forste_dato_ak_ss,  
    toDate = periode_data$siste_dato, 
    singleRow = FALSE)
  
  if(rendered_by_shiny){
    shiny::setProgress(0.60) 
  }
  
  anD_nasjonalt <- noric::getPrepAnDData(
    registryName = registryName, 
    fromDate  = periode_data$forste_dato,  
    toDate = periode_data$siste_dato, 
    singleRow = FALSE)
  
  
  if(rendered_by_shiny){
    shiny::setProgress(0.70) 
  }
  
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
  
  if(rendered_by_shiny){
    shiny::setProgress(0.80)
  }
  
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
  
  
  if(rendered_by_shiny){
    shiny::setProgress(0.90) 
  }
  
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
  
  if(rendered_by_shiny){
    shiny::setProgress(0.95) 
  }
  # LAGRE STAGING DATA
  rapbase::saveStagingData(
    registryName = registryName,
    dataName = paste0("ki",
                      as.character(format(Sys.time(),
                                          format = "%Y%m%d:%H:%M"))), 
    data = list(aK_nasjonalt = aK_nasjonalt, 
                aP_nasjonalt = aP_nasjonalt, 
                periode_data = periode_data))
}



#' Title
#'
#' @param registryName 
#'
#' @return
#' @export
#'
#' @examples
makeStagingDataFrame <- function(registryName){
  
  rapbase::mtimeStagingData(registryName = registryName) %>% 
    as.data.frame()%>% 
    tibble::rownames_to_column(., var = "Staging data") %>%
    dplyr::rename(., "Dato"=".") %>% 
    dplyr::arrange(.data$Dato)
  
}



#' Process bulletins for staging data
#' 
#' make and save staging data. Check validity and send e-mail about
#' unvalid staging data . Delete old datasets-
#'
#' @param dataset 
#' @param orgName 
#' @param orgId 
#' @param registryName 
#' @param userFullName 
#' @param userRole 
#' @param userOperator 
#' @param rendered_by_shiny 
#'
#' @return
#' @export
#'
#' @examples
bulletinProcessorStaging <- function(dataset = "ki", 
                                     orgName = "unknown organization",
                                     orgId = 999999,
                                     registryName = "noric",
                                     userFullName = "unknown person name",
                                     userRole = "unknown role",
                                     userOperator = "unknown operator", 
                                     rendered_by_shiny = FALSE){
  
  # Lage datasett
  noric::makeStagingDataKi(registryName = registryName)
  
  # sjekke at nyeste er godkjent (ny nok)
  
  # Sende e post dersom ikke godkjent
  
  # slette de som er over 1 uke gamle (kun dersom nyeste er godkjent)
  
  
}