#' NORIC PCI prom 
#' 
#' Hjelpefunksjoner for PCI prom
#'
#' @param df pci + pciprom
#' @param registryName for SQL queries
#'
#' @name prom_funksjoner
#' @aliases kriterie_taviprom_siste_aar  kriterie_pciprom_siste_aar kriterie_ingen_ny_tavi
#' 
#' 
#' @rdname prom_funksjoner
#' @export
kriterie_taviprom_siste_aar <- function(df, registryName = NULL){
  
  # alle PID for pasienter med pci som har de øvrige kriteriene for pci-prom 
  # og som har prosedyredato etter prodsetting av pci-prom
  df_sub <- df  %>%  
    dplyr::filter(kriterie_alle %in% "ja", 
                  ProsedyreDato >= as.Date("2026-05-27", format = "%Y-%m-%d"))  %>%  
    dplyr::select(PasientID, ProsedyreDato, ForlopsID) 
  
  if(nrow(df_sub)>0){
    
    list_patients <- df_sub  %>%  
      dplyr::distinct(PasientID)  %>%  
      dplyr::pull(PasientID)  %>%  
      stringr::str_flatten_comma(last = ", ")
    
    # Alle TAVI-prom for de utvalgte pasientene, men ingen kontroll på dato ennå
    query = paste0("
           SELECT
             P.ID AS PasientID,
             proms.TSSENDT AS ePromBestillingsdato_tavi
           FROM
             proms
           INNER JOIN
             mce MCE ON proms.MCEID = MCE.MCEID
           INNER JOIN
             patient P ON MCE.PATIENT_ID = P.ID
           WHERE
             proms.REGISTRATION_TYPE LIKE 'TAVI%'
           AND
             proms.TSSENDT > '2025-05-27'
           AND
             P.ID IN ",
                   "(", list_patients, ")",
                   " ;")

    proms_tavi <- rapbase::loadRegData(
      registryName = registryName,
      query = query)
    
    # Dersom TAVI-eProm er bestilt i vinduet 
    # [9-8mnd FØR pci-prosedyren; 3-4mnd ETTER pci prosedyren]
    # så blir kriterie_taviprom = nei
    df_sub %<>% dplyr::select(ProsedyreDato, PasientID)  %>%
      dplyr::inner_join(., 
                        proms_tavi, 
                        by = "PasientID",
                        relationship = "many-to-many") %>%
      dplyr::mutate(
        teoretisk_pciProm_lower = ProsedyreDato %m-% months(9), 
        teoretisk_pciProm_upper = ProsedyreDato %m+% months(4), 
        kriterie_taviprom = ifelse(
          ePromBestillingsdato_tavi < teoretisk_pciProm_upper &
            ePromBestillingsdato_tavi > teoretisk_pciProm_lower,
          "nei", "ja"))
    
    return(df %>%dplyr::left_join(., 
                                  df_sub  %>%  dplyr::select(kriterie_taviprom, PasientID), 
                                  by = "PasientID"))
  } 
  if(nrow(df_sub)== 0) {
    return(df  %>%  dplyr::mutate(kriterie_taviprom = "ja"))
  }
}





#' @rdname prom_funksjoner
#' @export
kriterie_pciprom_siste_aar <- function(df, registryName = NULL){
  
  # alle PID for pasienter med pci som har de øvrige kriteriene for pci-prom 
  # og som har prosedyredato etter prodsetting av pci-prom
  df_sub <- df  %>%  
    dplyr::filter(kriterie_alle %in% "ja", 
                  ProsedyreDato >= as.Date("2026-05-27", format = "%Y-%m-%d"))  %>% 
    dplyr::select(PasientID, ProsedyreDato, ForlopsID) 
  
  if(nrow(df_sub)>0){
    
    list_patients <- df_sub  %>%  
      dplyr::distinct(PasientID)  %>% 
      dplyr::pull(PasientID)  %>% 
      stringr::str_flatten_comma(last = ", ")
    
    # Alle PCI-prom for de utvalgte pasientene, men ingen kontroll på dato ennå
    query <- paste0("
           SELECT
             P.ID AS PasientID,
             MCE.MCEID AS ForlopsID_pciProm,
             proms.TSSENDT AS ePromBestillingsdato_pci
           FROM
             proms
           INNER JOIN
             mce MCE ON proms.MCEID = MCE.MCEID
           INNER JOIN
             patient P ON MCE.PATIENT_ID = P.ID
           WHERE
             proms.REGISTRATION_TYPE LIKE 'PCI%'
           AND
             proms.TSSENDT > '2025-05-27'
           AND
             P.ID IN ",
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
    df_sub %<>% dplyr::select(ProsedyreDato, PasientID, ForlopsID)  %>%
      dplyr::inner_join(., 
                        proms_pci, 
                        by = "PasientID",
                        relationship = "many-to-many") %>%
      dplyr::filter(ForlopsID != ForlopsID_pciProm) %>%
      dplyr::mutate(
        teoretisk_pciProm_lower = ProsedyreDato %m-% months(9), 
        teoretisk_pciProm_upper = ProsedyreDato %m+% months(4), 
        kriterie_pciprom = ifelse(
          ePromBestillingsdato_pci >= teoretisk_pciProm_lower & 
            ePromBestillingsdato_pci <= teoretisk_pciProm_upper,
          "nei", "ja"))
    
    return(df %>% dplyr::left_join(., 
                                  df_sub  %>%  dplyr::select(kriterie_pciprom, PasientID), 
                                  by = "PasientID"))
  } 
  if(nrow(df_sub)== 0) {
    return(df  %>%  dplyr::mutate(kriterie_pciprom = "ja"))
  }
}




#' @rdname prom_funksjoner
#' @export
kriterie_ingen_ny_tavi <- function(df, registryName = NULL){
  
  # alle PID for pasienter med pci som har de øvrige kriteriene for pci-prom 
  # og som har prosedyredato etter prodsetting av pci-prom
  df_sub <- df  %>%  
    dplyr::filter(kriterie_alle %in% "ja", 
                  ProsedyreDato >= as.Date("2026-05-27", format = "%Y-%m-%d"))  %>%  
    dplyr::select(PasientID, ProsedyreDato, ForlopsID) 
  
  if(nrow(df_sub)>0){
   
    list_patients <- df_sub  %>%  
      dplyr::distinct(PasientID)  %>%  
      dplyr::pull(PasientID)  %>%  
      stringr::str_flatten_comma(last = ", ")
    
    # Alle TAVI for de utvalgte pasientene, men ingen kontroll på dato ennå
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
    
     
    # Dersom PCI-eProm er bestilt i vinduet 
    # [9mnd FØR pci-prosedyren; 3mnd ETTER pci prosedyren]
    # så blir kriterie_taviprom = nei
    df_sub %<>% dplyr::select(ProsedyreDato, PasientID, ForlopsID)  %>%
      dplyr::inner_join(., 
                        nye_tavi, 
                        by = "PasientID",
                        relationship = "many-to-many") %>%
      dplyr::mutate(
        teoretisk_pciProm_upper = ProsedyreDato %m+% months(3), 
        kriterie_ingen_ny_tavi = ifelse(
          ProsedyreDato_tavi >= ProsedyreDato &
            ProsedyreDato_tavi <= teoretisk_pciProm_upper, 
          "nei", "ja"))
    
    return(df %>% dplyr::left_join(., 
                                   df_sub  %>%  dplyr::select(kriterie_ingen_ny_tavi, PasientID), 
                                   by = "PasientID"))
  } 
  if(nrow(df_sub)== 0) {
    return(df  %>%  dplyr::mutate(kriterie_ingen_ny_tavi = "ja"))
  }
}



