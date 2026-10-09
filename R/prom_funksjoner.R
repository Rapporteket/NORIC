#' Hjelpefunksjoner for databehandling av PROM 
#' 
#' Gjeler PCI-prom og TAVI-prom 
#' 
#' For forløp der alle andre kriterier (alder, prosedyretype, indikasjon,
#'  utskrivelse, etc.) er oppfylt, det vil si
#'   \code{kriterie_alle_midlertidig} = \emph{ja}. 
#'  Sjekker om pasientene hadde eksisterende TAVI-PROM eller PCI-PROM det
#'  siste året. Sjekker om pasienten har fått TAVI i de 3 påfølgende månedene. 
#'  
#'  Legger til ny variabel: 
#'  
#' \code{kriterie_taviprom_siste_aar()}
#'  \itemize{
#'  \item \code{kriterie_ingen_taviprom har verdien} har verdien \emph{nei}
#'  dersom pasienten hadde TAVI-PROM det siste året.  
#'  }
#' \code{kriterie_pciprom_siste_aar()}
#'  \itemize{
#'  \item \code{kriterie_ingen_pciprom har verdien} har verdien \emph{nei}
#'  dersom pasienten hadde PCI-PROM det siste året.  
#'  }
#'  #' \code{kriterie_ingen_tavi_neste3mnd()}
#'  \itemize{
#'  \item \code{kriterie_ingen_ny_tavi har verdien} har verdien \emph{nei}
#'  dersom pasienten har vært til en TAVI-prosedyre i etterkant av PCI
#'   prosedyren (0-3mnd).
#'  }
#'  
#'
#' @param df Inneholder data fra pci/tavi + respetiktive prom.
#' 
#' Noen df må inneholde variabelen \code{kriterie_alle_midlertidig}. 
#' @param registryName for SQL queries
#'
#' @name prom_funksjoner
#' @aliases kriterie_taviprom_siste_aar  kriterie_pciprom_siste_aar kriterie_ingen_tavi_neste3mnd legg_til_promStatus legg_til_promErrorCode
#' 
#' 
#' @rdname prom_funksjoner
#' @export
kriterie_taviprom_siste_aar <- function(df, registryName = NULL){
  
  stopifnot(c("kriterie_alle_midlertidig") %in% names(df))
  
  # alle PID for pasienter med pci som har de øvrige kriteriene for pci-prom 
  # og som har prosedyredato etter prodsetting av pci-prom
  df_sub <- df  %>%  
    dplyr::filter(
      kriterie_alle_midlertidig %in% "ja", 
      ProsedyreDato >= as.Date("2026-05-27", format = "%Y-%m-%d")) %>%  
    dplyr::select(PasientID, ProsedyreDato, ForlopsID, kriterie_alle_midlertidig) 
  
  if(nrow(df_sub)>0){
    
    list_patients <- df_sub  %>%  
      dplyr::distinct(PasientID)  %>%  
      dplyr::pull(PasientID)  %>%  
      stringr::str_flatten_comma(last = ", ")
    
    # Alle TAVI-prom for de utvalgte pasientene
    query = paste0("
           SELECT
             MCE.PATIENT_ID AS PasientID,
             proms.TSSENDT AS ePromBestillingsdato_tavi
           FROM
             proms
           INNER JOIN
             mce MCE ON proms.MCEID = MCE.MCEID
           WHERE
             proms.REGISTRATION_TYPE LIKE 'TAVI%'
           AND
             proms.TSSENDT > '2025-05-27'
           AND
             MCE.PATIENT_ID IN ",
                   "(", list_patients, ")",
                   " ;")
    
    proms_tavi <- rapbase::loadRegData(
      registryName = registryName,
      query = query)
    
    # Dersom TAVI-eProm er bestilt i vinduet 
    # [9-8mnd FØR pci-prosedyren; 3-4mnd ETTER pci prosedyren]
    # så blir kriterie_taviprom = nei
    df_sub %<>% 
      dplyr::filter(
        ProsedyreDato >= as.Date("2026-05-27", format = "%Y-%m-%d")) %>%
      dplyr::inner_join(., 
                        proms_tavi, 
                        by = "PasientID",
                        relationship = "one-to-many") %>%
      dplyr::filter(
        kriterie_alle_midlertidig %in% "ja", 
        ProsedyreDato >= as.Date("2026-05-27", format = "%Y-%m-%d")) %>%  
      dplyr::mutate(
        teoretisk_pciProm_lower = ProsedyreDato %m-% months(9), 
        teoretisk_pciProm_upper = ProsedyreDato %m+% months(3), 
        kriterie_ingen_taviprom = ifelse(
          ePromBestillingsdato_tavi < teoretisk_pciProm_upper &
            ePromBestillingsdato_tavi > teoretisk_pciProm_lower,
          "nei", NA_character_))
    
    return(
      dplyr::left_join(
        df, 
        df_sub  %>%  dplyr::select(kriterie_ingen_taviprom, PasientID, ForlopsID), 
        by = c("PasientID", "ForlopsID"))
      )
  } 
  if(nrow(df_sub)== 0) {
    return(df  %>%  dplyr::mutate(kriterie_ingen_taviprom = NA_character_))
  }
}





#' @rdname prom_funksjoner
#' @export
kriterie_pciprom_siste_aar <- function(df, registryName = NULL){

  stopifnot(c("kriterie_alle_midlertidig") %in% names(df))
  
    
  # alle PID for pasienter med pci som har de øvrige kriteriene for pci-prom 
  # og som har prosedyredato etter prodsetting av pci-prom
  df_sub <- df  %>%  
    dplyr::filter(
      kriterie_alle_midlertidig %in% "ja", 
      ProsedyreDato >= as.Date("2026-05-27", format = "%Y-%m-%d")) %>% 
    dplyr::select(PasientID, ProsedyreDato, ForlopsID, kriterie_alle_midlertidig) 
  
  if(nrow(df_sub)>0){
    
    list_patients <- df_sub  %>%  
      dplyr::distinct(PasientID)  %>% 
      dplyr::pull(PasientID)  %>% 
      stringr::str_flatten_comma(last = ", ")
    
    # Alle PCI-prom for de utvalgte pasientene, men ingen kontroll på dato ennå
    query <- paste0("
           SELECT
             MCE.PATIENT_ID AS PasientID,
             MCE.MCEID AS ForlopsID_pciProm,
             proms.TSSENDT AS ePromBestillingsdato_pci
           FROM
             proms
           INNER JOIN
             mce MCE ON proms.MCEID = MCE.MCEID
           WHERE
             proms.REGISTRATION_TYPE LIKE 'PCI%'
           AND
             proms.TSSENDT > '2025-05-27'
           AND
             MCE.PATIENT_ID IN ",
                    "(", list_patients, ")",
                    " ;")
    
    proms_pci <- rapbase::loadRegData(
      registryName = registryName,
      query = query)  %>%  
      dplyr::mutate(ePromBestillingsdato_pci = as.Date(ePromBestillingsdato_pci, 
                                                       format = "%Y-%m-%d"))
    
    # Dersom PCI-eProm er bestilt i vinduet 
    # [9mnd FØR pci-prosedyren; 3mnd ETTER pci prosedyren]
    # så blir kriterie_taviprom = nei
    df_sub %<>% 
      dplyr::inner_join(., 
                        proms_pci, 
                        by = "PasientID",
                        relationship = "many-to-many") %>%
      dplyr::filter(ForlopsID != ForlopsID_pciProm) %>%
      dplyr::mutate(
        teoretisk_pciProm_lower = ProsedyreDato %m-% months(9), 
        teoretisk_pciProm_upper = ProsedyreDato %m+% months(3), 
        kriterie_ingen_pciprom = ifelse(
          ePromBestillingsdato_pci >= teoretisk_pciProm_lower & 
            ePromBestillingsdato_pci <= teoretisk_pciProm_upper,
          "nei", NA_character_))
    
    return(
      dplyr::left_join(
        df, 
        df_sub %>% dplyr::select(kriterie_ingen_pciprom, PasientID, ForlopsID), 
        by = c("PasientID", "ForlopsID"))
    )
  } 
  if(nrow(df_sub)== 0) {
    return(df %>%  dplyr::mutate(kriterie_ingen_pciprom = NA_character_))
  }
}




#' @rdname prom_funksjoner
#' @export
kriterie_ingen_tavi_neste3mnd <- function(df, registryName = NULL){
  stopifnot(c("kriterie_alle_midlertidig") %in% names(df))
  
  # alle PID for pasienter med pci som har de øvrige kriteriene for pci-prom 
  # og som har prosedyredato etter prodsetting av pci-prom
  df_sub <- df  %>%  
    dplyr::filter(
      kriterie_alle_midlertidig %in% "ja", 
      ProsedyreDato >= as.Date("2026-05-27", format = "%Y-%m-%d"))  %>%  
    dplyr::select(PasientID, ProsedyreDato, ForlopsID, kriterie_alle_midlertidig) 
  
  if(nrow(df_sub)>0){
    
    list_patients <- df_sub  %>%  
      dplyr::distinct(PasientID)  %>%  
      dplyr::pull(PasientID)  %>%  
      stringr::str_flatten_comma(last = ", ")
    
    # Alle TAVI for de utvalgte pasientene
    query = paste0("
                   SELECT
                      MCE.PATIENT_ID AS PasientID,
                      MCE.INTERDAT AS ProsedyreDato_tavi
                   FROM
                      mce MCE
                   WHERE
                      MCE.INTERVENTION_TYPE = 5
                   AND
                      MCE.INTERDAT >= '2026-05-27'
                   AND
                   MCE.PATIENT_ID IN ",
                   "(", list_patients, ")",
                   " ;")
    
    nye_tavi <- rapbase::loadRegData(
      registryName = registryName,
      query = query)
    
    df_sub %<>% 
      dplyr::inner_join(., 
                        nye_tavi, 
                        by = "PasientID",
                        relationship = "many-to-many") %>%
      dplyr::filter(ProsedyreDato <= ProsedyreDato_tavi)  %>%
      dplyr::mutate(
        teoretisk_pciProm_upper = ProsedyreDato %m+% months(3), 
        kriterie_ingen_ny_tavi = ifelse(
          ProsedyreDato_tavi >= ProsedyreDato &
            ProsedyreDato_tavi <= teoretisk_pciProm_upper, 
          "nei", NA_character_))
    
    return(
      dplyr::left_join(
        df, 
        df_sub %>% dplyr::select(kriterie_ingen_ny_tavi, PasientID, ForlopsID), 
        by = c("PasientID", "ForlopsID"))
    )
  } 
  if(nrow(df_sub)== 0) {
    return(df %>% dplyr::mutate(kriterie_ingen_ny_tavi = NA_character_))
  }
}

#' @rdname prom_funksjoner
#' @export
legg_til_promStatus <- function(df){
  stopifnot("ePromStatus" %in% names(df))
  
  df %>% dplyr::mutate(ePromStatus_tekst= dplyr::case_when(
    ePromStatus %in% 0 ~ "created", 
    ePromStatus %in% 1 ~ "ordered", 
    ePromStatus %in% 2 ~ "expired", 
    ePromStatus %in% 3 ~ "completed", 
    ePromStatus %in% c(4,6) ~ "failed",
    TRUE ~ NA_character_)) %>% 
    dplyr::relocate(ePromStatus_tekst, .after = ePromStatus)
}

#' @rdname prom_funksjoner
#' @export
legg_til_promErrorCode <- function(df){
  stopifnot("form_order_status_error_code" %in% names(df))
  
  df %>% dplyr::mutate(form_order_status_error_code_tekst = dplyr::case_when(
    form_order_status_error_code %in% -1 ~ "unknown", 
    form_order_status_error_code %in% 0 ~ "none", 
    form_order_status_error_code %in% 1 ~ "patient unreachable", 
    form_order_status_error_code %in% 2 ~ "sikker digital post error", 
    TRUE ~ NA_character_)) %>% 
    dplyr::relocate(form_order_status_error_code_tekst, 
                    .after = form_order_status_error_code)
}
