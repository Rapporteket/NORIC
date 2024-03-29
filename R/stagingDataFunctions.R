#' NORIC staging data functions
#'
#' NORIC's handling of staging data at Rapporteket.
#'
#' @details
#'
#' \itemize{
#' \item \code{makeStagingDataKi()} creates staging data for the KI-report.
#' \item \code{makeStagingDataFrame()} lists all of the existing staging data.
#' \item \code{checkValidStagingData()} checks whether any staging data set has
#'  been created within the last \code{diffDaysCheck} days. Returns
#' \code{valid_staging_data} is TRUE and the name of the most recently created
#' data set \code{nyeste_staging_data} if this is the case, and FALSE elsewhere.
#' \item \code{deleteOldStagingData()} deletes staging data older than
#' \code{diffDaysDelete}
#' \item \code{bulletinProcessorStaging()} is a modified version of
#' \code{reportProcessor()}. The function is applied to create bulletins for
#' staged data.
#' }
#'
#' @param registryName Character string defining the registry name.
#' @param rendered_by_shiny boolean. if TRUE progression of pdf-generation is
#'  returned.
#' @param diffDaysCheck numerical. Default values is 0 (today).
#'   \code{checkValidStagingData()} checks if any staging data has been created
#'   within the \code{diffDaysCheck} days.
#' @param diffDaysDelete numerical. No default value.
#'  \code{deleteOldStagingData()} deletes staging data older than this.
#' @param dataset Which kind of staging data to create
#' @param orgName Character string with the name of the organization/hospital.
#'   Default is "unknown organization".
#' @param orgId Integer (?) with the id of the organization/hospital. Default
#'   is 999999.
#' @param userFullName Character string giving the person name, normally the
#'   user requesting the report. Default is "unknown person name".
#' @param userRole Character string giving a user role, normally the one of the
#'   user requesting the report. Default is "unknown role".
#' @param userOperator Character string with some name of an operator, whatever
#'   that is... Default is "unknown operator".
#' @param author "ukjent"
#
#' @name stagingData
#' @aliases makeStagingDataKi
#' makeStagingDataFrame
#' checkValidStagingData
#' deleteOldStagingData
#' bulletinProcessorStaging
NULL

#' @rdname stagingData
#' @export
makeStagingDataKi <- function(registryName, rendered_by_shiny = FALSE) {
  
  
  if (rendered_by_shiny) {
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
  # SS: Brukes til "antall_stent_under_opphold" (foreskriving av medikamenter)
  # + "stenting_av_venstre_hovedstamme" (IVUS/OCT - Per kvartal!)
  
  
  periode_data <- data.frame(
    # Nyeste registrering eller gårsdagen (ikke forhåndsregistreringer)
    nyeste_reg = min(
      (as.Date(Sys.time()) - 1),
      noric::getLatestEntry(registryName = registryName))) %>%
    
    dplyr::mutate(
      
      # SISTE DATO AVGJØR SISTE MÅNED:
      siste_dato_innevarende_mnd = lubridate::ceiling_date(
        x = nyeste_reg, 
        unit =  "month") - lubridate::days(1),
      
      siste_dato = dplyr::case_when(
        # siste dato er nyeste dato --> nyeste mnd er komlett
        siste_dato_innevarende_mnd == nyeste_reg ~ nyeste_reg,
        
        # siste dato er ikke nyeste --> nyest mnd er ikke komplett, ta forrige
        siste_dato_innevarende_mnd != nyeste_reg ~
          lubridate::floor_date(nyeste_reg, "month") -
          lubridate::days(1),
        
        TRUE ~ as.Date(NA_character_)),
      
      
      
      
      # SISTE DATO AVGJØR SISTE KVARTAL
      siste_dato_innevarende_kvartal = lubridate::ceiling_date(
        x = nyeste_reg,
        unit =  "quarter") - lubridate::days(1),
      
      kvartal_komplett_start = dplyr::case_when(
        # siste dato i kvartalet er lik nyeste dato --> kvartalet er komplett
        siste_dato_innevarende_kvartal == nyeste_reg ~
          lubridate::floor_date(nyeste_reg, unit =  "quarter"),
        
        # siste dato i kvartalet er ulik nyeste dato -->bruk forrige kvartal
        siste_dato_innevarende_kvartal != nyeste_reg ~
          lubridate::floor_date(nyeste_reg, unit =  "quarter") - months(3),
        
        TRUE ~ as.Date(NA_character_)),
      # Inneværende år:
      nyesteRegYear = as.numeric(format(siste_dato, format = "%Y")),
      
      # Fjoråret
      sisteHeleYear = nyesteRegYear - 1,
      
      # Første dato for SQL -spørring : 01. januar fjoråret
      forste_dato  = as.Date(paste0(sisteHeleYear, "-01-01"),
                             format = "%Y-%m-%d"),
      
      # Aortaklaff + Segment stent (indikator per kvartal):
      forste_dato_ak_ss = as.Date(paste0(sisteHeleYear -1, "-01-01"),
                                  format = "%Y-%m-%d")
    ) %>%
    dplyr::select(-siste_dato_innevarende_mnd,
                  -siste_dato_innevarende_kvartal)
  
  
  if (rendered_by_shiny) {
    shiny::setProgress(0.10)
  }
  
  # HENTE DATA:
  sS_nasjonalt <- noric::getPrepSsData(
    registryName = registryName,
    fromDate  = periode_data$forste_dato_ak_ss,
    toDate = periode_data$siste_dato,
    singleRow = FALSE)
  
  if (rendered_by_shiny) {
    shiny::setProgress(0.20)
  }
  
  # Hardkodet 2018- dags dato pga figur 4,5 og 6
  aP_nasjonalt <- noric::getPrepApData(
    registryName = registryName,
    fromDate  = as.Date("2018-01-01", format = "%Y-%m-%d"),
    toDate = periode_data$siste_dato,
    singleRow = FALSE)
  
  if (rendered_by_shiny) {
    shiny::setProgress(0.50)
  }
  
  aK_nasjonalt <- noric::getPrepAkData(
    registryName = registryName,
    fromDate  = periode_data$forste_dato_ak_ss,
    toDate = periode_data$siste_dato,
    singleRow = FALSE)
  
  if (rendered_by_shiny) {
    shiny::setProgress(0.60)
  }
  
  if (rendered_by_shiny) {
    shiny::setProgress(0.70)
  }
  
  # BEARBEIDE DATA:
  sS_nasjonalt %<>%
    dplyr::select(ProsedyreDato,
                  StentType,
                  Segment,
                  Graft,
                  ForlopsID,
                  Sykehusnavn,
                  AvdRESH,
                  aar,
                  maaned,
                  kvartal)
  
  if (rendered_by_shiny) {
    shiny::setProgress(0.80)
  }
  
  aP_nasjonalt %<>%
    dplyr::select(
      AvdRESH,
      Sykehusnavn,
      Regtype,
      PrimaerForlopsID,
      ProsedyreDato,
      ProsedyreTid,
      ProsedyreType,
      OverflyttetFra,
      AnkomstPCIDato,
      AnkomstPCITid,
      InnleggelseHenvisendeSykehusDato,
      InnleggelseHenvisendeSykehusTid,
      Innkomstarsak,
      Indikasjon,
      Hastegrad,
      BesUtlEKGDato,
      BesUtlEKGTid,
      BeslutningsutlosendeEKG,
      BeslEKGDato, 
      BeslEKGTid,
      GittTrombolyse,
      HLRForSykehus,
      KobletForlopsID,
      ForlopsType2,
      IFR,
      FFR,
      IVUS,
      OCT,
      IMR, 
      PDPA, 
      PA_Hyperemi,
      PD_Hyperemi,
      ForlopsID,
      ASA,
      AndrePlatehemmere,
      AndrePlatehemmere,
      Antikoagulantia,
      UtskrStatiner,
      TidlABC,
      UtskrevetDod,
      SkjemaStatusStart,
      SkjemastatusHovedskjema,
      SkjemaStatusUtskrivelse,
      SkjemaStatusKomplikasjoner
    ) %>%
    
    noric::utlede_OppholdsID(.) %>%
    
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
                              suffix = "KomplikSkjema") %>%
    
    noric::legg_til_antall_stent(df_ap = ., df_ss = sS_nasjonalt) %>%
    noric::legg_til_antall_stent_opphold(df_ap = .) %>%
    noric::satt_inn_stent_i_lms(df_ap = ., df_ss = sS_nasjonalt) %>%
    
 
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
  
  
  if (rendered_by_shiny) {
    shiny::setProgress(0.90)
  }
  
  aK_nasjonalt %<>%
    dplyr::select(
      ProsedyreDato,
      AvdKompPacemaker,
      LabKompDod,
      TypeKlaffeprotese,
      Sykehusnavn,
      AvdRESH,
      ForlopsID,
      Pacemaker,
      SkjemaStatusHovedskjema)
  
  aK_nasjonalt %<>%
    dplyr::mutate(
      kvartal = paste0(
        lubridate::year(ProsedyreDato),
        " Q",
        lubridate::quarter(ProsedyreDato, with_year = FALSE))) %>%
    noric::ki_ak_pacemakerbehov(df_ak = .)
  
  if (rendered_by_shiny) {
    shiny::setProgress(0.95)
  }
  
  
  aP_Shus <- aP_nasjonalt %>% 
    dplyr::select(AvdRESH) %>% 
    dplyr::distinct(AvdRESH) %>% 
    dplyr::pull()
  
  aK_Shus <- aK_nasjonalt %>% 
    dplyr::select(AvdRESH) %>% 
    dplyr::distinct(AvdRESH) %>% 
    dplyr::pull()
  
  
  # LAGRE STAGING DATA
  stagingDataFilename <- paste0("ki",
                                as.character(format(Sys.time(),
                                                    format = "%Y%m%d:%H:%M")))
  rapbase::saveStagingData(
    registryName = registryName,
    dataName = stagingDataFilename,
    data = list(aK_nasjonalt = aK_nasjonalt,
                aP_nasjonalt = aP_nasjonalt,
                periode_data = periode_data, 
                aP_Shus = aP_Shus, 
                aK_Shus = aK_Shus))
  
  return(stagingDataFilename)
}



#' @rdname stagingData
#' @export
makeStagingDataFrame <- function(registryName) {
  
  rapbase::mtimeStagingData(registryName = registryName) %>%
    as.data.frame()%>%
    tibble::rownames_to_column(., var = "Staging data") %>%
    dplyr::rename(., "Dato"=".")

}



#' @rdname stagingData
#' @export
checkValidStagingData <- function(registryName, diffDaysCheck = 0) {
  
  
  valid_staging_data <- FALSE
  nyeste_staging_data <- FALSE
  
  n_staging <- length(rapbase::listStagingData(registryName = registryName)) 
  
  # Dersom minst en staging data, sjekk validitet og returner nyeste
  if (n_staging > 0) {
    
    stagingData <- noric::makeStagingDataFrame(registryName = registryName) 
    
    # For hvert datasett, sjekk at det er nytt nok
    stagingData %<>% dplyr::mutate(
      nytt_nok = ifelse(
        test = as.numeric(difftime(time1 = Sys.Date(),
                                   time2 = as.Date(.data$Dato),
                                   units = "days")) <= diffDaysCheck,
        yes = TRUE,
        no = FALSE), 
      ap_shus_ok = FALSE, 
      ak_shus_ok = FALSE)
    
    # Behold bare de som er nye nok:
    stagingData %<>% 
      dplyr::filter(.data$nytt_nok) %>% 
      dplyr::arrange(dplyr::desc(.data$Dato))
    
    
  if ((stagingData %>% nrow()) > 0) { 
    # for hvert datasett, sjekk at alle 10 AP sykehusene er representert og 
    # de 5 AK sykehusene
    i <- 1
    for(i in 1:(stagingData %>% nrow())) {
      tmp_staging <- rapbase::loadStagingData(
        registryName = registryName, 
        dataName = stagingData$'Staging data'[i])
      
      aP_Shus <- tmp_staging$aP_Shus
      
      aK_Shus <- tmp_staging$aK_Shus
      
      stagingData$ap_shus_ok[i] <- ifelse(
        test = all(c(102966, 101619, 104284, 105502, 106944,
                        108141, 109880, 114150, 4210141, 700422) %in% aP_Shus), 
                   yes = TRUE, 
                   no = FALSE)
      
      stagingData$ak_shus_ok[i] <- ifelse(
        test = all(c(102966, 700422,109880, 104284, 101619) %in% aK_Shus), 
                   yes = TRUE, 
                   no = FALSE)
      
      i <- i + 1  
    }
    
    # Beholde kun de som er gyldige
    stagingData %<>% 
      dplyr::filter(.data$ap_shus_ok, 
                    .data$ak_shus_ok) %>% 
      dplyr::arrange(dplyr::desc(.data$Dato))
    }
    
    

    # Bland gyldige, behold nyeste
    valid_staging_data <-  ifelse(test = stagingData %>% nrow() > 0,
                                  yes = TRUE, 
                                  no= FALSE)
    
    nyeste_staging_data <- ifelse(test = valid_staging_data,
                                  yes = stagingData$'Staging data'[1],
                                  no = FALSE)
  }
  
  
  return(list(valid_staging_data = valid_staging_data,
              nyeste_staging_data = nyeste_staging_data))
  
}

#' @rdname stagingData
#' @export
deleteOldStagingData <- function(registryName, diffDaysDelete) {
  
  
  allStaging <- noric::makeStagingDataFrame(registryName = registryName)
  
  
  n_old <-  allStaging %>%
    dplyr::filter(.data$Dato <= (Sys.Date() - diffDaysDelete )) %>%
    nrow()
  
  if(n_old > 0){
    oldStaging <- allStaging %>%
      dplyr::filter(.data$Dato <= (Sys.Date() - diffDaysDelete)) %>%
      dplyr::pull(.data$'Staging data')
    
    i <- 1
    for(i in 1:n_old){
      rapbase::deleteStagingData(registryName = registryName,
                                 dataName = oldStaging[i])
      i <- i +1
    }
  }
  
  
}

#' @rdname stagingData
#' @export
bulletinProcessorStaging <- function(dataset = "ki",
                                     orgName = "unknown organization",
                                     orgId = 999999,
                                     registryName = "noric",
                                     userFullName = "unknown person name",
                                     userRole = "unknown role",
                                     userOperator = "unknown operator",
                                     rendered_by_shiny = FALSE,
                                     author = "ingen") {
  
  # Lage datasett
  melding_start <- "Denne bulletin'en laget ingen datasett. "
  
  if (dataset %in% "ki") {
    stagingDataFilename <- noric::makeStagingDataKi(
      registryName = registryName)
    
    melding_start <- paste0("Denne bulletin'en laget: ",
                                  stagingDataFilename, ". ")
  }
  
  
  # sjekke om det finnes et godkjent staging data set og dette er dagens
  #  se om det er likt som nyeste navn. 
  # Maks 6 dager tilbake i tid
  sjekkStaging <- noric::checkValidStagingData(registryName = registryName,
                                               diffDaysCheck = 6)
  
  if(sjekkStaging$valid_staging_data){
    melding_sjekk <- paste0("--> Sjekk OK, nyeste gyldige datasett er: ", 
                           sjekkStaging$nyeste_staging_data)
    
    if(! sjekkStaging$nyeste_staging_data %in% stagingDataFilename) {
      melding_sjekk <- paste0(" MERK at nyeste er ikke gyldig!", melding_sjekk)
    }
    
  } else {
    melding_sjekk <- " --> Sjekk utført, ingen datasett er gyldige."
  }
  
  
  # slette de som er over 1 uke gamle (kun dersom nyeste er godkjent)
  if (sjekkStaging$valid_staging_data) {
    noric::deleteOldStagingData(registryName = registryName,
                                diffDaysDelete = 7)
    
  }
  
  meldingstekst <- paste0(melding_start, melding_sjekk)
  
  
  # meldingstekst <- ifelse(
  #   test = sjekkStaging$valid_staging_data,
  #   yes = paste0(melding_start,
  #                "--> Sjekk OK, nyeste datasett er: ",
  #                sjekkStaging$nyeste_staging_data),
  #   no = paste0(melding_start,
  #               "--> Sjekk ikke OK, ingen datasett er gyldige."))
  # 
  # 
  # returnerer meldingen som skal sendes på e-post
  return(meldingstekst)
}
