#' getAKOppfData provides local reg data from AortaklaffOppfVar
#'
#' @param registryName String providing the registry name
#' @param singleRow Logical defining if only one row is to be returned. A
#' relevant usecase will be when only description is needed. By default set to
#' FALSE
#' @param ... Optional arguments to be passed to the function
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter mutate mutate_all select left_join
#' @importFrom lubridate ymd year month quarter isoweek
#'
#' @return Data frame representing the table AortaklaffOppfVar
#' @export
#'

getAKOppfData <- function(registryName, singleRow = FALSE, ...) {
  
  # declare 'dot'
  . <- ""
  
  dbType <- "mysql"
  query <-"
SELECT *
FROM AortaklaffOppfVar
  "
  
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg = "Query metadata for AortaklaffOppfVar pivot"
  } else {
    query <- paste0(query, ";")
    msg = "Query data for AortaklaffOppfVar pivot"
  }
  
  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  AKOppf <- rapbase::LoadRegData(registryName, query, dbType)
  
  FO <- rapbase::LoadRegData(registryName,
                             query = "SELECT * FROM ForlopsOversikt")
  
  
  # Velger relevante variabler fra FO som skal legges til tabellen:
  FO %<>% 
    select(
      # Nøkler:
      AvdRESH
      ,ForlopsID
      
      # Variablene som legges til:
      ,Sykehusnavn
      ,PasientID
      ,PasientKjonn
      ,PasientAlder
      ,BasisRegStatus
      ,ForlopsType1
      ,ForlopsType2
      ,KobletForlopsID
      ,HovedDato
      ,Kommune
      ,KommuneNr
      ,Fylke
      ,Fylkenr
      ,FodselsDato
      ,Avdod
      ,AvdodDato
      ,ErOppflg
      ,OppflgStatus
      ,OppflgSekNr
      ,OppflgRegStatus
    )
  
  # Legger til variabler fra FO til AKOppf:
  AKOppf <- left_join(AKOppf, FO, by = c("AvdRESH"
                                         ,"ForlopsID"
                                         )
  )
  
  
  
  # Gjor datoer om til dato-objekt:
  AKOppf %<>%
    mutate(
      BasisBeslutningsDato = ymd( BasisBeslutningsDato )
      ,BasisProsedyreDato = ymd( BasisProsedyreDato )
      ,HovedDato = ymd( HovedDato )
      ,FodselsDato = ymd( FodselsDato )
      ,OppfDato = ymd( OppfDato )
      ,OppfAvdodDato = ymd( OppfAvdodDato ) # kan hende denne er identisk til AvdodDato
      ,AvdodDato = ymd( AvdodDato )
    )
  
         
  
  # Endre Sykehusnavn til kortere versjoner:
  AKOppf %<>%
    mutate(
      Sykehusnavn = ifelse( Sykehusnavn == "Haukeland" , "HUS" , Sykehusnavn ) ,
      Sykehusnavn = ifelse( Sykehusnavn %in% c("St.Olav", "St. Olav") , "St.Olavs"  , Sykehusnavn ) ,
      Sykehusnavn = ifelse( Sykehusnavn == "Akershus universitetssykehus HF" , "Ahus" , Sykehusnavn )
    )
  
  
  # Tar bort forløp fra før sykehusene ble offisielt med i NORIC (potensielle
  # "tøyseregistreringer")
  AKOppf %<>%
    filter(
      (
        (Sykehusnavn=="HUS") & ( as.Date(ProsedyreDato) >= "2013-01-01") # Unødvendig å bruke as.Date(), slette senere?
      ) | (
        (Sykehusnavn=="UNN") & ( as.Date(ProsedyreDato) >= "2013-05-01" )
      ) | (
        (Sykehusnavn=="Ullevål") & ( as.Date(ProsedyreDato) >= "2014-01-01" )
      ) | (
        (Sykehusnavn=="St.Olavs") & ( as.Date(ProsedyreDato) >= "2014-01-01" )
      ) | (
        (Sykehusnavn=="Sørlandet") & ( as.Date(ProsedyreDato) >= "2014-01-01" )
      ) | (
        (Sykehusnavn=="SUS") & ( as.Date(ProsedyreDato) >= "2014-01-01" )
      ) | (
        (Sykehusnavn=="Rikshospitalet") & ( as.Date(ProsedyreDato) >= "2015-01-01" )
      ) | (
        (Sykehusnavn=="Feiring") & ( as.Date(ProsedyreDato) >= "2015-01-01" )
      ) | (
        (Sykehusnavn=="Ahus") & ( as.Date(ProsedyreDato) >= "2016-01-01" )
      ))
  
  
  # Gjøre kategoriske variabler om til factor:
  # (ikke fullstendig, må legge til mer etter hvert)
  AKOppf %<>%
    mutate(
      ForlopsType1 = as.ordered( ForlopsType1 )
      ,ForlopsType2 = factor( ForlopsType2,
                              levels = c(
                                "Akutt"
                                , "Subakutt"
                                , "Planlagt"
                              ),
                              ordered = TRUE )
      ,Sykehusnavn = as.ordered( Sykehusnavn )
      ,PasientKjonn = factor(PasientKjonn, 
                             levels = c( 
                               "Mann"
                               , "Kvinne"
                               , NA
                             )
                             ,ordered = TRUE
                             ,exclude = NULL # inkluderer NA i levels
      )
      ,VektUkjent = as.ordered( VektUkjent )
      ,NYHA = as.ordered( NYHA )
      ,CanadianClass = as.ordered( CanadianClass )
      ,GangtestIkkeUtfort = as.ordered( GangtestIkkeUtfort )
      ,SKreatininIkkeUtfort = as.ordered( SKreatininIkkeUtfort )
      ,HemoglobinUkjent = as.ordered( HemoglobinUkjent )
      ,VenstreVentrikkelFunksjon = as.ordered( VenstreVentrikkelFunksjon )
      ,Aortainsuffisiens = as.ordered( Aortainsuffisiens )
      ,ParavalvularLekkasje = as.ordered( ParavalvularLekkasje )
      ,Mitralinsuffisiens = as.ordered( Mitralinsuffisiens )
      ,Komplikasjoner = as.ordered( Komplikasjoner )
      
      ,UtskrevetTil = factor(UtskrevetTil, 
                             levels = c( 
                               "Hjem"           
                               ,"Rehabilitering" 
                               ,"Annet sykehus"
                               ,"Sykehjem"
                               , NA
                             )
                             ,exclude = NULL # inkluderer NA i levels
                             ,ordered = TRUE
      )
      
    )
  
  
  # Utledete variabler:
  AKOppf %<>%
    mutate(
      # Div. tidsvariabler:
      
      # Basert på BasisProsedyreDato:
      # Kalenderår:
      year_prosedyre = as.ordered( year( BasisProsedyreDato )),
      aar_prosedyre = year_prosedyre,
      # Måned:
      # (månedsnr er tosifret; 01, 02, ....)
      maaned_nr_prosedyre = as.ordered( sprintf(fmt = "%02d", month( BasisProsedyreDato ) )),
      maaned_prosedyre = as.ordered( paste0( year_prosedyre, "-", maaned_nr_prosedyre) ),
      # Kvartal:
      kvartal_prosedyre = quarter( BasisProsedyreDato, with_year = TRUE ),
      # kvartal = as.factor( gsub( "\\.", "-", kvartal) ),
      kvartal_prosedyre = as.ordered( gsub( "[[:punct:]]", "-Q", kvartal_prosedyre) ),
      # Uketall:
      uke_prosedyre = as.ordered( sprintf(fmt = "%02d", isoweek( BasisProsedyreDato ) )),

      # Variabel "yyyy-ukenummer" som tar høyde for uketall som befinner seg i to kalenderår:
      aar_uke_prosedyre = ifelse( test = uke_prosedyre == "01" & maaned_nr_prosedyre == "12", # hvis uke 01 i desember...
                        yes = paste0( as.integer(year(BasisProsedyreDato)) + 1, "-", uke_prosedyre ), # ..sier vi at year er det seneste året som den uken tilhørte
                        no = paste0(aar_prosedyre, "-", uke_prosedyre )
      ),
      aar_uke_prosedyre = ifelse( test = uke_prosedyre %in% c("52", "53") & maaned_nr_prosedyre == "01", # hvis uke 52 eller 53 i januar...
                        yes = paste0( as.integer(year(BasisProsedyreDato)) - 1, "-", uke_prosedyre ), # ...sier vi at hele uken tilhører det tidligste året
                        no = aar_uke_prosedyre
      ),
      aar_uke_prosedyre = as.ordered( aar_uke_prosedyre ),
      
      # Basert på OppfDato:
      # Kalenderår:
      year_oppfolging = as.ordered( year( OppfDato )),
      aar_oppfolging = year_oppfolging,
      # Måned:
      # (månedsnr er tosifret; 01, 02, ....)
      maaned_nr_oppfolging = as.ordered( sprintf(fmt = "%02d", month( OppfDato ) )),
      maaned_oppfolging = as.ordered( paste0( year_oppfolging, "-", maaned_nr_oppfolging) ),
      # Kvartal:
      kvartal_oppfolging = quarter( OppfDato, with_year = TRUE ),
      # kvartal = as.factor( gsub( "\\.", "-", kvartal) ),
      kvartal_oppfolging = as.ordered( gsub( "[[:punct:]]", "-Q", kvartal_oppfolging) ),
      # Uketall:
      uke_oppfolging = as.ordered( sprintf(fmt = "%02d", isoweek( OppfDato ) )),

      # Variabel "yyyy-ukenummer" som tar høyde for uketall som befinner seg i to kalenderår:
      aar_uke_oppfolging = ifelse( test = uke_oppfolging == "01" & maaned_nr_oppfolging == "12", # hvis uke 01 i desember...
                        yes = paste0( as.integer(year(OppfDato)) + 1, "-", uke_oppfolging ), # ..sier vi at year er det seneste året som den uken tilhørte
                        no = paste0(aar_oppfolging, "-", uke_oppfolging )
      ),
      aar_uke_oppfolging = ifelse( test = uke_oppfolging %in% c("52", "53") & maaned_nr_oppfolging == "01", # hvis uke 52 eller 53 i januar...
                        yes = paste0( as.integer(year(OppfDato)) - 1, "-", uke_oppfolging ), # ...sier vi at hele uken tilhører det tidligste året
                        no = aar_uke_oppfolging
      ),
      aar_uke_oppfolging = as.ordered( aar_uke_oppfolging )
    )
  
  
  
  
  AKOppf
  
}