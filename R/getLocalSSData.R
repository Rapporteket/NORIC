#' getLocalSSData provides local reg data from SegmentStent
#'
#' @param registryName String providing the registry name
#' @param singleRow Logical defining if only one row is to be returned. A
#' relevant usecase will be when only description is needed. By default set to
#' FALSE
#' @param ... Optional arguments to be passed to the function
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter mutate mutate_all select group_by count left_join
#' arrange
#' @importFrom lubridate ymd year month quarter isoweek
#'
#' @return Data frame representing the table SegmentStent
#' @export
#'

getLocalSSData <- function(registryName, singleRow = FALSE, ...) {
  
  # declare 'dot'
  . <- ""
  
  dbType <- "mysql"
  query <-"
SELECT
  *
FROM
  SegmentStent
"
  
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg = "Query metadata for SegmentStent pivot"
  } else {
    query <- paste0(query, ";")
    msg = "Query data for SegmentStent pivot"
  }
  
  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  SS <- rapbase::LoadRegData(registryName, query, dbType)
  
  FO <- rapbase::LoadRegData(registryName,
                             query = "SELECT * FROM ForlopsOversikt")
  
  # Velger relevante variabler fra FO som skal legges til tabellen:
  FO %<>% 
    select(
      # Nøkler:
      AvdRESH
      ,ForlopsID
      ,Sykehusnavn
      # Variablene som legges til:
      ,PasientID
      # ,FodselsDato # Finnes per d.d. i SS
      ,Kommune
      ,KommuneNr
      ,Fylke
      ,Fylkenr
      # ,PasientKjonn # Finnes per d.d. i SS
      ,PasientAlder
      ,ForlopsType1
      ,ForlopsType2
      ,KobletForlopsID
      ,HovedDato
    )
  
  SS <- left_join(SS, FO, by = c("ForlopsID", "AvdRESH", "Sykehusnavn"),
                  suffix = c("", ".FO"))
  
  # Gjor datoer om til dato-objekt:
  SS %<>%
    mutate(
      FodselsDato = ymd( FodselsDato )
      ,HovedDato = ymd( HovedDato )
      ,ProsedyreDato = ymd( ProsedyreDato )
    )
  
  
  # Endre Sykehusnavn til kortere versjoner:
  SS %<>%
    mutate(
      Sykehusnavn = ifelse( Sykehusnavn == "Haukeland" , "HUS" , Sykehusnavn ) ,
      Sykehusnavn = ifelse( Sykehusnavn %in% c("St.Olav", "St. Olav") , "St.Olavs"  , Sykehusnavn ) ,
      Sykehusnavn = ifelse( Sykehusnavn == "Akershus universitetssykehus HF" , "Ahus" , Sykehusnavn )
    )
  
  # Tar bort forløp fra før sykehusene ble offisielt med i NORIC (potensielle
  # "tøyseregistreringer")
  SS %<>%
    filter(
      (
        (AvdRESH == 102966) & ( as.Date(ProsedyreDato) >= "2013-01-01" ) # HUS
      ) | (
        (AvdRESH == 101619) & ( as.Date(ProsedyreDato) >= "2013-05-01" ) # UNN
      ) | (
        (AvdRESH == 109880) & ( as.Date(ProsedyreDato) >= "2014-01-01" ) # Ullevål
      ) | (
        (AvdRESH == 104284) & ( as.Date(ProsedyreDato) >= "2014-01-01" ) # St.Olavs
      ) | (
        (AvdRESH == 114150) & ( as.Date(ProsedyreDato) >= "2014-01-01" ) # SSA
      ) | (
        (AvdRESH == 105502) & ( as.Date(ProsedyreDato) >= "2014-01-01" ) # SUS
      ) | (
        (AvdRESH == 700422) & ( as.Date(ProsedyreDato) >= "2015-01-01" ) # Riksen
      ) | (
        (AvdRESH == 106944) & ( as.Date(ProsedyreDato) >= "2015-01-01" ) # LHLGardermoen
      ) | (
        (AvdRESH == 108141) & ( as.Date(ProsedyreDato) >= "2016-01-01" ) # Ahus
      ))
  
  
  # Gjøre kategoriske variabler om til factor:
  # (ikke fullstendig, må legge til mer etter hvert)
  SS %<>%
    mutate(
      Etterdilatasjon = factor( Etterdilatasjon,
                          levels = c(
                            "Ja"
                            , "Ja, gjennom stentmaskene"
                            , "Ja, Kissing-Balloon teknikk"
                            , "Ja, separate ballonger gjennom stentmaskene og i stent"
                            , "Nei"
                            , "Ukjent"
                            , NA)
                          ,exclude = NULL # inkluderer NA i levels
                          ,ordered = TRUE
      ),
      Indikasjon = as.factor( Indikasjon ),
      Graft = factor( Graft,
                      levels = c(
                        "Nei"
                        ,"Arteriell"
                        ,"Vene"
                      )
                      ,exclude = NULL # inkluderer NA i levels
                      ,ordered = TRUE 
      ),
      LokalSuksess = factor( LokalSuksess,
                             levels = c(
                               "Ja"
                               ,"Nei"
                               ,NA
                             )
                             ,exclude = NULL # inkluderer NA i levels
                             ,ordered = TRUE 
      ),
      PasientKjonn = factor(PasientKjonn, 
                            levels = c( 
                              "Mann"
                              , "Kvinne"
                              , NA
                            )
                            ,exclude = NULL # inkluderer NA i levels
                            ,ordered = TRUE
      ),
      ProsedyreType = factor( ProsedyreType,
                              levels = c(
                                "Annen terapi"
                                ,"Atherectomi"
                                ,"Ballong + Stent"
                                ,"Ballongdilatasjon"
                                ,"Cutting Ballon"
                                ,"Diagnostikk"
                                ,"Direktestent"
                                ,"Medikamentell ballong"
                                ,"Medikamentell ballong + Stent"
                                ,"Rotablator"
                                ,"Wireforsøk"
                              ),
                              ordered = TRUE 
                              ),
      Segment = as.ordered( Segment ),
      Stenoseklasse = factor( Stenoseklasse,
                              levels = c(
                                "A"
                                ,"B1"
                                ,"B1 Bifurkasjon"
                                ,"B2"
                                ,"B2 Bifurkasjon"
                                ,"C"
                                ,"C Bifurkasjon"
                                ,"Annet"
                              ),
                              ordered = TRUE 
                              ),
      StenoseType = factor( StenoseType,
                              levels = c(
                                "DeNovo"
                                ,"In-stent restenose"
                                ,"Stenttrombose "
                                ,"Andre restenoser"
                              ),
                              ordered = TRUE 
                              ),
      StentType = factor( StentType,
                          levels = c(
                            "DES"
                            , "BMS"
                            , "Annet"
                            , NA)
                          ,exclude = NULL # inkluderer NA i levels
                          ,ordered = TRUE
      ),
      Sykehusnavn = as.ordered( Sykehusnavn )
      
    )
  
  
  # Utledete variabler:
  SS %<>% 
    mutate( 
      # Div. tidsvariabler:
      #
      # Kalenderår for ProsedyreDato:
      year = as.ordered( year( ProsedyreDato )),
      aar = year,
      # Måned:
      # (månedsnr er tosifret; 01, 02, ....)
      maaned_nr = as.ordered( sprintf(fmt = "%02d", month( ProsedyreDato ) )),
      maaned = as.ordered( paste0( year, "-", maaned_nr) ),
      # Kvartal:
      kvartal = quarter( ProsedyreDato, with_year = TRUE ),
      # kvartal = as.factor( gsub( "\\.", "-", kvartal) ),
      kvartal = as.ordered( gsub( "[[:punct:]]", "-Q", kvartal) ),
      # Uketall:
      uke = as.ordered( sprintf(fmt = "%02d", isoweek( ProsedyreDato ) )),
      
      # Variabel "yyyy-ukenummer" som tar høyde for uketall som befinner seg i to kalenderår:
      aar_uke = ifelse( test = uke == "01" & maaned_nr == "12", # hvis uke 01 i desember...
                        yes = paste0( as.integer(year(ProsedyreDato)) + 1, "-", uke ), # ..sier vi at year er det seneste året som den uken tilhørte
                        no = paste0(aar, "-", uke )
      ),
      aar_uke = ifelse( test = uke %in% c("52", "53") & maaned_nr == "01", # hvis uke 52 eller 53 i januar...
                        yes = paste0( as.integer(year(ProsedyreDato)) - 1, "-", uke ), # ...sier vi at hele uken tilhører det tidligste året
                        no = aar_uke
      ),
      aar_uke = as.ordered( aar_uke )
    )
  
  
  # Utledet variabel - ant_stent_ila_forlop = antall stenter satt inn ila ett forløp
  
  ant_stent <- SS  %>%
    group_by(Sykehusnavn) %>%
    count( ForlopsID, wt = !is.na( StentType ) )
  
  # Har nå en df med Sykehusnavn, ForlopsID og n = antall rader tilhørende et
  # forløp hvor StentType er oppgitt (!is.na() == TRUE når det er satt inn stent)

  # Endrer navn på "n":
  names( ant_stent )[3] <- "ant_stent_ila_forlop"
  
  # Legger "ant_stent_ila_forlop" SS vha en left join:
  SS %<>% 
    left_join(., ant_stent
                     , by = c("Sykehusnavn", "ForlopsID" ) 
                     ) %>% 
    arrange( Sykehusnavn, ForlopsID)
  
  # For hver rad blir det oppgitt antall stenter som ble satt inn ila det
  # forløpet (ett forløp på ett sykehus kan ha flere rader hvor hver rad oppgir det totale
  # antallet)
  
  
  SS
  
}