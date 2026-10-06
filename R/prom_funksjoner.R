#' NORIC PCI prom 
#' 
#' Hjelpefunksjoner for PCI prom
#'
#' @param df pci + pciprom
#'
#' @name prom_funksjoner
#' @aliases legg_til_taviStatus legg_til_taviErrorCode
#' 
#' 
#' @rdname prom_funksjoner
#' @export
antall_taviprom_siste_aar <- function(df, df_prom){
  
  # count number of TAVI-prom last 1 year before prosedyredato
  
  list_patients <- df |> 
    dplyr::filter(kriterie_alle %in% "ja", 
                  ProsedyreDato >= as.Date("2026-05-27", format = "%Y-%m-%d")) |> 
    dplyr::pull(PasientID) |> 
    stringr::str_flatten_comma(last = ", ")
 
  list_patients <- df |> 
    dplyr::filter(kriterie_alle %in% "ja", 
                  ProsedyreDato >= as.Date("2026-05-27", format = "%Y-%m-%d")) |> 
    dplyr::pull(PasientID) |> 
    stringr::str_flatten_comma(last = ", ")
  
  pci %<>% 
    dplyr::left_join(., 
                     proms_tavi |> dplyr::filter(ID %in% list_patients_vec, 
                                                 REGISTRATION_TYPE %in% "TAVI_samleskjema"), 
                     by = c("PasientID" = "ID"))
  
  proms_tavi <- rapbase::loadRegData(
    registryName = "noric_bergen", 
    query = paste0("
  SELECT 
    P.ID AS PasientID,  
    proms.TSSENDT AS ePromBestillingsdato 
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
    "('", list_patients, "')", 
  " ;"))
  
  

  
  
}
