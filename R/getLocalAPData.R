#' getLocalAPData provides local reg data from AngioPCIVar
#'
#' @param registryName 
#'
#' @return Data frame representing the table AndreProsedyrerVar
#' @export
#'

getLocalAPData <- function(registryName, ...) {
  
  dbType <- "mysql"
  APQuery <- "SELECT * FROM AngioPCIVar"
  
  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]],
                      msg = "Query data for AngioPCI pivot")
  }
  
  AP <- rapbase::LoadRegData(registryName, APQuery, dbType)
  
  showN <- 12 # how many months are displayed
  
  AP$FodselsDato <- as.Date( AP$FodselsDato)
  
  AP$InnleggelseHenvisendeSykehusDato <- as.Date( AP$InnleggelseHenvisendeSykehusDato)
  
  AP$AnkomstPCIDato <- as.Date( AP$AnkomstPCIDato)
  
  AP$ProsedyreDato <- as.Date( AP$ProsedyreDato)
  
  AP$Sykehusnavn <- factor( AP$Sykehusnavn )
  
  AP $ AdmissionType <- car::recode(
    var = AP $ OverflyttetFra ,
    recodes = "
        'Annet sykehus'='Referred';
        '' = NA;
        'Annen  avdeling på sykehuset' = NA;
        'Nei, direkte inn til dette sykehus' = 'Directly admitted';
        'Omdirigert ambulanse' = 'Directly admitted';
        ")
  
  AP $ Indikasjon2 <- factor(
    car::recode(
      var = AP $ Indikasjon ,
      recodes = "
            'Stabil koronarsykdom'='SAP';
            'UAP'='UAP';
            'NSTEMI'='NSTEMI';
            'STEMI'='STEMI';
            'Hjertestans ved STEMI'='STEMI';
            'STEMI > 24h'='STEMI';
            'STEMI/Rescue PCI'='STEMI';
            'Uklare brystsmerter'='Uklare brystsmerter';
            else='Annet';
            ") ,
    levels = c("Uklare brystsmerter","SAP","UAP","NSTEMI","STEMI","Annet") )
  
  
  AP$Funn[which(AP$Funn %in% c("","Ikke konklusiv undersøkelse"))] <- NA
  AP$NormaleKar <- as.numeric(AP$Funn == "Normalt /Ateromatos")
  
  AP$Year <- as.numeric(
    format(
      x = AP $ ProsedyreDato ,
      format = "%Y"))
  
  AP$Week <- as.numeric(
    format(
      x = AP $ ProsedyreDato ,
      format = "%W"))
  
  
  AP$nMonth <- as.numeric(
    as.factor(
      format(
        AP$ProsedyreDato ,
        format = "%y-%m")))
  
  AP $ Day <- as.numeric(
    AP $ ProsedyreDato - min( AP $ ProsedyreDato , na.rm = TRUE ) )
  
  NSTEMI <- subset(
    x = AP ,
    subset = (Indikasjon == "NSTEMI"))
  
  NSTEMI $ Month <- as.factor(
    format(
      x = NSTEMI $ ProsedyreDato ,
      format = "%y-%m"))
  
  
  AP <- subset(
    x = AP ,
    subset = nMonth >= max( nMonth , na.rm = TRUE ) - showN )
  
  # crude fix for Feiring and Rikshospitalet which have meaningless test data before 2015
  if (AP$Sykehusnavn[1] %in% c("Feiring","Rikshospitalet")) AP <- AP [ which( AP$Year >= 2015 ) , ]
  
  AP $ Month <- as.factor(
    format(
      x = AP $ ProsedyreDato ,
      format = "%y-%m"))
  
  
  AP

  }